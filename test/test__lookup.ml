(* This test exercises the lookup mechanism of the provider library. It includes
   a case with a sufficient number of traits (`A` to `F`) to ensure good test
   coverage of the various branches of the binary search lookup method.

   Additionally, this test features an example where multiple traits implement
   the same functionality (printing a tag) but with different behavior (each
   trait prints a different tag). This serves to demonstrate how a function of a
   library (`print_tag`) can dynamically select the trait based on the value of
   some parameter.

   The testing of the lookup mechanism and the demonstration of dynamic trait
   selection are orthogonal. They are combined in this test purely for testing
   purposes. *)

module type S = sig
  type t

  val print_tag : t -> unit
end

module Tag = struct
  [@@@coverage off]

  type t =
    [ `A
    | `B
    | `C
    | `D
    | `E
    | `F
    ]
  [@@deriving enumerate, sexp_of]
end

type 'a t = ([> Tag.t ] as 'a) Provider.t

type (_, _, _) Provider.Trait.t +=
  | A : ('a, (module S with type t = 'a), [> `A ]) Provider.Trait.t
  | B : ('a, (module S with type t = 'a), [> `B ]) Provider.Trait.t
  | C : ('a, (module S with type t = 'a), [> `C ]) Provider.Trait.t
  | D : ('a, (module S with type t = 'a), [> `D ]) Provider.Trait.t
  | E : ('a, (module S with type t = 'a), [> `E ]) Provider.Trait.t
  | F : ('a, (module S with type t = 'a), [> `F ]) Provider.Trait.t

module Selector = struct
  type 'a t = T : ('a, (module S with type t = 'a), Tag.t) Provider.Trait.t -> 'a t

  let of_tag = function
    | `A -> T A
    | `B -> T B
    | `C -> T C
    | `D -> T D
    | `E -> T E
    | `F -> T F
  ;;
end

let print_tag (Provider.T { t; interface } : _ t) ~tag =
  match Selector.of_tag tag with
  | T trait ->
    let module M = (val Provider.Interface.lookup interface ~trait) in
    M.print_tag t
;;

module Impl (M : sig
    val tag : Tag.t
  end) =
struct
  type t = unit

  let print_tag () = print_s [%sexp (M.tag : Tag.t)]
end

module Impls = struct
  module A = Impl (struct
      let tag = `A
    end)

  module B = Impl (struct
      let tag = `B
    end)

  module C = Impl (struct
      let tag = `C
    end)

  module D = Impl (struct
      let tag = `D
    end)

  module E = Impl (struct
      let tag = `E
    end)

  module F = Impl (struct
      let tag = `F
    end)
end

let provider () : _ t =
  let interface =
    Provider.Interface.make
      [ Provider.Trait.implement A ~impl:(module Impls.A)
      ; Provider.Trait.implement B ~impl:(module Impls.B)
      ; Provider.Trait.implement C ~impl:(module Impls.C)
      ; Provider.Trait.implement D ~impl:(module Impls.D)
      ; Provider.Trait.implement E ~impl:(module Impls.E)
      ; Provider.Trait.implement F ~impl:(module Impls.F)
      ]
  in
  Provider.T { t = (); interface }
;;

let%expect_test "lookup" =
  let (Provider.T { t = _; interface } as t) = provider () in
  print_s [%sexp (List.length (Provider.Interface.implementations interface) : int)];
  [%expect {| 6 |}];
  List.iter Tag.all ~f:(fun tag -> print_tag t ~tag);
  [%expect {|
    A
    B
    C
    D
    E
    F |}];
  ()
;;

(* In this section we show how to check at runtime that an interface implements
   a subset of another. This demonstrates other possible uses for trait unique
   ids. *)

let provider2 () : _ t =
  let interface =
    Provider.Interface.make
      [ Provider.Trait.implement A ~impl:(module Impls.A)
      ; Provider.Trait.implement B ~impl:(module Impls.B)
      ; Provider.Trait.implement D ~impl:(module Impls.D)
      ; Provider.Trait.implement F ~impl:(module Impls.F)
      ]
  in
  Provider.T { t = (); interface }
;;

module Uid = struct
  type t = Provider.Trait.Uid.t

  include Comparable.Make (Provider.Trait.Uid)
end

let uids (Provider.T { t = _; interface }) =
  interface
  |> Provider.Interface.implementations
  |> List.map ~f:Provider.Trait.Implementation.uid
  |> Set.of_list (module Uid)
;;

let%expect_test "sub-interface" =
  let traits1 = provider () |> uids in
  let traits2 = provider2 () |> uids in
  print_s [%sexp (Set.equal traits1 traits2 : bool)];
  [%expect {| false |}];
  print_s [%sexp (Set.is_subset traits2 ~of_:traits1 : bool)];
  [%expect {| true |}];
  ()
;;

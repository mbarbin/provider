(* This test exercises the lookup mechanism of the provider library. It includes
   a case with a sufficient number of classes (`A` to `F`) to ensure good test
   coverage of the various branches of the binary search lookup method.

   Additionally, this test features an example where multiple classes implement
   the same functionality (printing a tag) but with different behavior (each
   class prints a different tag). This serves to demonstrate how a function of a
   library (`print_tag`) can dynamically select the class based on the value of
   some parameter.

   The testing of the lookup mechanism and the demonstration of dynamic class
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

type (_, _, _) Provider.Class_id.t +=
  | A : ('a, (module S with type t = 'a), [> `A ]) Provider.Class_id.t
  | B : ('a, (module S with type t = 'a), [> `B ]) Provider.Class_id.t
  | C : ('a, (module S with type t = 'a), [> `C ]) Provider.Class_id.t
  | D : ('a, (module S with type t = 'a), [> `D ]) Provider.Class_id.t
  | E : ('a, (module S with type t = 'a), [> `E ]) Provider.Class_id.t
  | F : ('a, (module S with type t = 'a), [> `F ]) Provider.Class_id.t

module Selector = struct
  type 'a t = T : ('a, (module S with type t = 'a), Tag.t) Provider.Class_id.t -> 'a t

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
  | T class_id ->
    let module M = (val Provider.Interface.lookup interface ~class_id) in
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
      [ Provider.Class.implement ~class_id:A (module Impls.A)
      ; Provider.Class.implement ~class_id:B (module Impls.B)
      ; Provider.Class.implement ~class_id:C (module Impls.C)
      ; Provider.Class.implement ~class_id:D (module Impls.D)
      ; Provider.Class.implement ~class_id:E (module Impls.E)
      ; Provider.Class.implement ~class_id:F (module Impls.F)
      ]
  in
  Provider.T { t = (); interface }
;;

let%expect_test "lookup" =
  let (Provider.T { t = _; interface } as t) = provider () in
  print_s [%sexp (List.length (Provider.Interface.classes interface) : int)];
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

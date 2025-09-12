(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

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

type 'a t = ([> Tag.t ] as 'a) Provider.packed

module C_all () = Provider.Trait.Create (struct
    type 't module_type = (module S with type t = 't)
  end)

module A = C_all ()
module B = C_all ()
module C = C_all ()
module D = C_all ()
module E = C_all ()
module F = C_all ()

let a : (_, _, [> `A ]) Provider.Trait.t = A.t
let b : (_, _, [> `B ]) Provider.Trait.t = B.t
let c : (_, _, [> `C ]) Provider.Trait.t = C.t
let d : (_, _, [> `D ]) Provider.Trait.t = D.t
let e : (_, _, [> `E ]) Provider.Trait.t = E.t
let f : (_, _, [> `F ]) Provider.Trait.t = F.t

module Selector = struct
  type 'a t = T : ('a, (module S with type t = 'a), Tag.t) Provider.Trait.t -> 'a t

  let of_tag = function
    | `A -> T a
    | `B -> T b
    | `C -> T c
    | `D -> T d
    | `E -> T e
    | `F -> T f
  ;;
end

let print_tag (Provider.T { t; provider } : _ t) ~tag =
  match Selector.of_tag tag with
  | T trait ->
    let module M = (val Provider.lookup provider ~trait) in
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
  let provider =
    Provider.make
      [ Provider.implement a ~impl:(module Impls.A)
      ; Provider.implement b ~impl:(module Impls.B)
      ; Provider.implement c ~impl:(module Impls.C)
      ; Provider.implement d ~impl:(module Impls.D)
      ; Provider.implement e ~impl:(module Impls.E)
      ; Provider.implement f ~impl:(module Impls.F)
      ]
  in
  Provider.T { t = (); provider }
;;

let%expect_test "lookup" =
  let (Provider.T { t = _; provider } as t) = provider () in
  print_s [%sexp (List.length (Provider.bindings provider) : int)];
  [%expect {| 6 |}];
  List.iter Tag.all ~f:(fun tag -> print_tag t ~tag);
  [%expect
    {|
    A
    B
    C
    D
    E
    F |}];
  ()
;;

(* In this section we show how to check at runtime that a provider implements a
   subset of another. This demonstrates other possible uses for trait unique
   ids. *)

let provider2 () : _ t =
  let provider =
    Provider.make
      [ Provider.implement a ~impl:(module Impls.A)
      ; Provider.implement b ~impl:(module Impls.B)
      ; Provider.implement d ~impl:(module Impls.D)
      ; Provider.implement f ~impl:(module Impls.F)
      ]
  in
  Provider.T { t = (); provider }
;;

module Uid = struct
  type t = Provider.Trait.Uid.t

  include Comparable.Make (Provider.Trait.Uid)
end

let uids (Provider.T { t = _; provider }) =
  provider
  |> Provider.bindings
  |> List.map ~f:Provider.Binding.uid
  |> Set.of_list (module Uid)
;;

let%expect_test "sub-provider" =
  let traits1 = provider () |> uids in
  let traits2 = provider2 () |> uids in
  print_s [%sexp (Set.equal traits1 traits2 : bool)];
  [%expect {| false |}];
  print_s [%sexp (Set.is_subset traits2 ~of_:traits1 : bool)];
  [%expect {| true |}];
  ()
;;

let provider3 () : _ t =
  let provider =
    Provider.make
      [ Provider.implement a ~impl:(module Impls.A)
      ; Provider.implement c ~impl:(module Impls.C)
      ; Provider.implement e ~impl:(module Impls.E)
      ; Provider.implement f ~impl:(module Impls.F)
      ]
  in
  Provider.T { t = (); provider }
;;

let%expect_test "same_trait_uids" =
  (* This exercises the test when the provider arrays have the same length,
     otherwise we skip the actual uid comparison branch. *)
  let same_trait_uids
        (Provider.T { t = _; provider = h1 })
        (Provider.T { t = _; provider = h2 })
    =
    print_s [%sexp (Provider.Private.same_trait_uids h1 h2 : bool)]
  in
  let p1 = provider () in
  let p2 = provider2 () in
  let p2_bis = provider2 () in
  let p3 = provider3 () in
  same_trait_uids p1 p1;
  [%expect {| true |}];
  same_trait_uids p1 p2;
  [%expect {| false |}];
  same_trait_uids p2 p2_bis;
  [%expect {| true |}];
  same_trait_uids p2 p3;
  [%expect {| false |}];
  ()
;;

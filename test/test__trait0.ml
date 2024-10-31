(* In this section we make sure to exercise all available [Create] functors and
   monitor the 2 branches of the [same_witness] functions. *)

let same_trait t1 t2 =
  match Provider.Private.Trait0.same_witness t1 t2 with
  | Equal -> true
  | Not_equal -> false
;;

let%expect_test "Create" =
  let module T1 =
    Provider.Trait.Create (struct
      type 'a module_type = (module T with type t = 'a)
    end)
  in
  let module T2 =
    Provider.Trait.Create (struct
      type 'a module_type = (module T with type t = 'a)
    end)
  in
  require [%here] (same_trait T1.t T1.t : bool);
  require [%here] (not (same_trait T1.t T2.t : bool));
  ()
;;

let%expect_test "Create0" =
  let module T1 =
    Provider.Trait.Create0 (struct
      type t = unit
      type module_type = unit
    end)
  in
  let module T2 =
    Provider.Trait.Create0 (struct
      type t = unit
      type module_type = unit
    end)
  in
  require [%here] (same_trait T1.t T1.t : bool);
  require [%here] (not (same_trait T1.t T2.t : bool));
  ()
;;

let%expect_test "Create1" =
  let module T1 =
    Provider.Trait.Create1 (struct
      type 'a t = 'a
      type 'a module_type = unit
    end)
  in
  let module T2 =
    Provider.Trait.Create1 (struct
      type 'a t = 'a
      type 'a module_type = unit
    end)
  in
  require [%here] (same_trait T1.t T1.t : bool);
  require [%here] (not (same_trait T1.t T2.t : bool));
  ()
;;

let%expect_test "Create2" =
  let module T1 =
    Provider.Trait.Create2 (struct
      type ('a, 'b) t = 'a * 'b
      type ('a, 'b) module_type = unit
    end)
  in
  let module T2 =
    Provider.Trait.Create2 (struct
      type ('a, 'b) t = 'a * 'b
      type ('a, 'b) module_type = unit
    end)
  in
  require [%here] (same_trait T1.t T1.t : bool);
  require [%here] (not (same_trait T1.t T2.t : bool));
  ()
;;

(* Below we add extra tests regarding Marshal, just for added context. They are
   not directly related to the project but are interesting context to have in
   mind while working on the representation of traits. *)

let%expect_test "Marshal extensible variant" =
  (* Marshalling breaks extensible variant matching, as explained in the OCaml
     documentation. *)
  let module E = struct
    type t = ..
    type t += A | B of int
  end
  in
  let open E in
  require [%here] (phys_equal A A);
  require [%here] (not (phys_equal A (B 0)));
  require [%here] (not (phys_equal (B 0) (B 0)));
  let id (t : t) =
    Stdlib.Obj.Extension_constructor.id (Stdlib.Obj.Extension_constructor.of_val t)
  in
  require [%here] (id (B 0) = id (B 0));
  require [%here] (id (B 0) = id (B 2));
  (* Marshalling does not preserve physical equality of extensible variant with
     no arguments. *)
  let marshal = Stdlib.Marshal.to_string A [] in
  let a2 = Stdlib.Marshal.from_string marshal 0 in
  require [%here] (not (phys_equal A a2));
  (* Marshalling does not preserve extension constructor ids. *)
  let marshal = Stdlib.Marshal.to_string (B 0) [] in
  let b2 = Stdlib.Marshal.from_string marshal 0 in
  require [%here] (id (B 0) <> id b2);
  ()
;;

let%expect_test "Marshal" =
  (* Because trait contain extensible variant, they inherit some behavior from
     they as it relates to marshalling traits.

     In short: do not marshall traits. *)
  let module T =
    Provider.Trait.Create (struct
      type 'a module_type = (module T with type t = 'a)
    end)
  in
  let t1 = T.t in
  require [%here] (same_trait t1 t1 : bool);
  let marshal = Stdlib.Marshal.to_string T.t [ Closures ] in
  let t2 = Stdlib.Marshal.from_string marshal 0 in
  require [%here] (same_trait t2 t2 : bool);
  require [%here] (not (same_trait t1 t2 : bool));
  ()
;;

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

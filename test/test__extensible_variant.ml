(* In this test, we aim at monitoring certain runtime properties related to
   variant and extensible variant in relation to how they're used by the
   library, in an attempt to have some awareness of whether the way we use them
   is correct. *)

module Obj = Stdlib.Obj

(* First let's characterize the representation of the Eq_opt constructors used
   by the provider implementation. It's a non-extensible GADT with two immediate
   constructors, represented by the integers {0, 1} at runtime. *)

module Eq_opt = struct
  type (_, _) t =
    | Equal : ('a, 'a) t
    | Not_equal : ('a, 'b) t
end

let%expect_test "Eq_opt at runtime" =
  let test obj =
    let is_int = Obj.is_int obj in
    require [%here] is_int;
    print_s [%sexp { is_int : bool; value = (Obj.obj obj : int) }]
  in
  test (Obj.repr Eq_opt.Equal);
  [%expect {|
    ((is_int true)
     (value  0))
    |}];
  test (Obj.repr Eq_opt.Not_equal);
  [%expect {|
    ((is_int true)
     (value  1))
    |}];
  ()
;;

(* Next we'll look at the representation of various Traits. We then go and
   attempt to characterize ways the API may be abused. *)

module No_arg_A : sig
  val t : ('t, (module T with type t = 't), [> `T ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 'a module_type = (module T with type t = 'a)
  end)

module No_arg_B : sig
  val t : ('t, (module T with type t = 't), [> `T ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 'a module_type = (module T with type t = 'a)
  end)

let () =
  Provider.Trait.Info.register_name No_arg_A.t ~name:"No_arg_A";
  Provider.Trait.Info.register_name No_arg_B.t ~name:"No_arg_B";
  ()
;;

let%expect_test "extension_constructor" =
  print_s [%sexp (Provider.Trait.info No_arg_A.t : Provider.Trait.Info.t)];
  [%expect {|
    ((id   #id)
     (name No_arg_A))
    |}];
  print_s [%sexp (Provider.Trait.info No_arg_B.t : Provider.Trait.Info.t)];
  [%expect {|
    ((id   #id)
     (name No_arg_B))
    |}];
  let extension_constructor_A = Obj.Extension_constructor.of_val No_arg_A.t in
  print_s [%sexp (Obj.Extension_constructor.name extension_constructor_A : string)];
  [%expect {| "Provider__Trait0.Create1(X).T" |}];
  let extension_constructor_B = Obj.Extension_constructor.of_val No_arg_B.t in
  print_s [%sexp (Obj.Extension_constructor.name extension_constructor_B : string)];
  [%expect {| "Provider__Trait0.Create1(X).T" |}];
  (* We do not print the actual runtime ids because it is too brittle. We simply
     characterize that they are different. *)
  let idA = Obj.Extension_constructor.id extension_constructor_A in
  let idB = Obj.Extension_constructor.id extension_constructor_B in
  require_not_equal [%here] (module Int) idA idB;
  [%expect {||}];
  ()
;;

let%expect_test "implement" =
  (* This test covers a case where [implement_unsafe ~check_trait] succeeds. *)
  let handler =
    Provider.Handler.make
      [ Provider.Trait.implement
          No_arg_A.t
          ~impl:
            (module struct
              type t = int
            end)
      ]
  in
  let module M = (val Provider.Handler.lookup handler ~trait:No_arg_A.t) in
  let x = (0 : M.t) in
  print_s [%sexp (x : int)];
  [%expect {| 0 |}];
  ()
;;

let%expect_test "no_arg physical equality" =
  require [%here] (phys_equal No_arg_A.t No_arg_A.t);
  [%expect {||}];
  require [%here] (phys_equal No_arg_B.t No_arg_B.t);
  [%expect {||}];
  require [%here] (not (phys_equal No_arg_A.t No_arg_B.t));
  [%expect {||}];
  let new_A () = No_arg_A.t in
  require [%here] (phys_equal (new_A ()) No_arg_A.t);
  [%expect {||}];
  require [%here] (phys_equal (new_A ()) (new_A ()));
  [%expect {||}];
  ()
;;

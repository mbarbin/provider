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

type ('t, 'module_type, 'tag) Provider.Trait.t +=
  | No_arg_A : ('t, (module T with type t = 't), [> `T ]) Provider.Trait.t
  | No_arg_B : ('t, (module T with type t = 't), [> `T ]) Provider.Trait.t

let%expect_test "extension_constructor" =
  print_s [%sexp (Provider.Trait.info No_arg_A : Provider.Trait.Info.t)];
  [%expect {| ((id #id) (name Provider_test.Test__extensible_variant.No_arg_A)) |}];
  print_s [%sexp (Provider.Trait.info No_arg_B : Provider.Trait.Info.t)];
  [%expect {| ((id #id) (name Provider_test.Test__extensible_variant.No_arg_B)) |}];
  let extension_constructor_A = Obj.Extension_constructor.of_val No_arg_A in
  print_s [%sexp (Obj.Extension_constructor.name extension_constructor_A : string)];
  [%expect {| Provider_test.Test__extensible_variant.No_arg_A |}];
  let extension_constructor_B = Obj.Extension_constructor.of_val No_arg_B in
  print_s [%sexp (Obj.Extension_constructor.name extension_constructor_B : string)];
  [%expect {| Provider_test.Test__extensible_variant.No_arg_B |}];
  (* We do not print the actual runtime ids because it is too brittle. We simply
     characterize that they are different. *)
  let idA = Obj.Extension_constructor.id extension_constructor_A in
  let idB = Obj.Extension_constructor.id extension_constructor_B in
  require_not_equal [%here] (module Int) idA idB;
  [%expect {||}];
  ()
;;

let%expect_test "no_arg physical equality" =
  require [%here] (phys_equal No_arg_A No_arg_A);
  [%expect {||}];
  require [%here] (phys_equal No_arg_B No_arg_B);
  [%expect {||}];
  require [%here] (not (phys_equal No_arg_A No_arg_B));
  [%expect {||}];
  let new_A () = No_arg_A in
  require [%here] (phys_equal (new_A ()) No_arg_A);
  [%expect {||}];
  require [%here] (phys_equal (new_A ()) (new_A ()));
  [%expect {||}];
  ()
;;

module Name_override = struct
  type ('t, 'module_type, 'tag) Provider.Trait.t +=
    | No_arg_A : ('t, (module T with type t = 't), [> `T ]) Provider.Trait.t
end

let%expect_test "name override" =
  require [%here] (not (phys_equal No_arg_A Name_override.No_arg_A));
  [%expect {||}];
  let extension_constructor_A = Obj.Extension_constructor.of_val No_arg_A in
  let extension_constructor_A' =
    Obj.Extension_constructor.of_val Name_override.No_arg_A
  in
  require_not_equal
    [%here]
    (module Int)
    (Obj.Extension_constructor.id extension_constructor_A)
    (Obj.Extension_constructor.id extension_constructor_A');
  [%expect {||}];
  ()
;;

module With_arg = struct
  type (_, _, _) extensible = ..

  type ('t, 'module_type, 'tag) extensible +=
    | A : { value : 'a } -> ('t, 't * 'a, [> `T ]) extensible
    | B : { value : 'a } -> ('t, 'a * 't, [> `T ]) extensible
end

let%expect_test "extension_constructor" =
  let extension_constructor_A =
    Obj.Extension_constructor.of_val (With_arg.A { value = 0 })
  in
  print_s [%sexp (Obj.Extension_constructor.name extension_constructor_A : string)];
  [%expect {| Provider_test.Test__extensible_variant.With_arg.A |}];
  let extension_constructor_A' =
    Obj.Extension_constructor.of_val (With_arg.A { value = "0" })
  in
  print_s [%sexp (Obj.Extension_constructor.name extension_constructor_A' : string)];
  [%expect {| Provider_test.Test__extensible_variant.With_arg.A |}];
  let extension_constructor_B =
    Obj.Extension_constructor.of_val (With_arg.B { value = "0" })
  in
  print_s [%sexp (Obj.Extension_constructor.name extension_constructor_B : string)];
  [%expect {| Provider_test.Test__extensible_variant.With_arg.B |}];
  let idA = Obj.Extension_constructor.id extension_constructor_A in
  let idA' = Obj.Extension_constructor.id extension_constructor_A' in
  let idB = Obj.Extension_constructor.id extension_constructor_B in
  require_equal [%here] (module Int) idA idA';
  [%expect {||}];
  require_not_equal [%here] (module Int) idA idB;
  [%expect {||}];
  ()
;;

let%expect_test "with_arg physical equality" =
  require [%here] (not (phys_equal (With_arg.A { value = 0 }) (With_arg.A { value = 0 })));
  [%expect {||}];
  ()
;;

(* Because we currently allow Traits with arguments, we have effectively the
   possibility for different traits to have the same id even though they are
   physically different. *)

module type S = sig
  type t

  val show : t -> string
end

type show = [ `Show ]

type (_, _, _) Provider.Trait.t +=
  | Show : { arg : int } -> ('t, (module S with type t = 't), [> show ]) Provider.Trait.t

let%expect_test "ids" =
  let trait1 = Show { arg = 0 } in
  let trait2 = Show { arg = 1 } in
  require [%here] (not (phys_equal trait1 trait2));
  [%expect {||}];
  let idA = Provider.Trait.uid (Show { arg = 0 }) in
  let idB = Provider.Trait.uid (Show { arg = 1 }) in
  require_equal [%here] (module Provider.Trait.Uid) idA idB;
  [%expect {||}];
  ()
;;

let print (Provider.T { t; handler }) =
  (let module M = (val Provider.Handler.lookup handler ~trait:(Show { arg = 0 })) in
  print_endline (M.show t)) [@coverage off]
;;

let string_provider t =
  let handler =
    Provider.Handler.make
      [ Provider.Trait.implement
          (Show { arg = 0 })
          ~impl:
            (module struct
              type t = string

              let show = Fn.id
            end)
      ]
  in
  Provider.T { t; handler }
;;

let%expect_test "invalid_trait" =
  require_does_raise [%here] (fun () -> print (string_provider "Hello World"));
  [%expect
    {|
    ("Invalid usage of [Provider.Trait]: Extensible variants with the same id are expected to be physically equal through the use of this library"
     ((
       trait (
         (id   #id)
         (name Provider_test.Test__extensible_variant.Show)))))
    |}];
  ()
;;

(* Note that the API may be changed in the future to detect this error sooner,
   or to avoid it by design. This is left as future work. *)

type ('t, 'module_type, 'tag) With_arg.extensible +=
  | With_arg_C :
      { a : 'a
      ; b : 'b
      }
      -> ('t, 'a * 'b, [> `T ]) With_arg.extensible

let%expect_test "with-arg detection" =
  (* To detect the presence of argument, we can check whether the object is the
     same as its extension constructor. *)
  let test (repr : Obj.t) =
    let is_block = Obj.is_block repr in
    require [%here] is_block;
    let size = Obj.size repr in
    let extension_constructor = Obj.Extension_constructor.of_val repr in
    let name = Obj.Extension_constructor.name extension_constructor in
    let has_args = not (phys_equal repr (Obj.repr extension_constructor)) in
    print_s [%sexp { name : string; is_block : bool; size : int; has_args : bool }]
  in
  test (Obj.repr No_arg_A);
  [%expect
    {|
    ((name Provider_test.Test__extensible_variant.No_arg_A)
     (is_block true)
     (size     2)
     (has_args false))
    |}];
  test (Obj.repr (With_arg.A { value = 42 }));
  [%expect
    {|
    ((name Provider_test.Test__extensible_variant.With_arg.A)
     (is_block true)
     (size     2)
     (has_args true))
    |}];
  test (Obj.repr (With_arg_C { a = 0; b = "1" }));
  [%expect
    {|
    ((name Provider_test.Test__extensible_variant.With_arg_C)
     (is_block true)
     (size     3)
     (has_args true))
    |}];
  ()
;;

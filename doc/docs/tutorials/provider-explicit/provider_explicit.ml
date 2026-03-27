(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

(* @mdexp

   # Provider Explicit

   In this tutorial, we draw a parallel between a way to use the provider library,
   and [modular explicit](https://gallium.inria.fr/~remy/ocamod/modular-explicits.pdf).

   ## Introduction

   Modular-explicit allows *module-dependent* functions to take a module
   implementing a Trait signature as an argument and use a type from the module
   to annotate subsequent arguments. For example: *)

module Ocaml_toplevel = Provider_toplevel_test.Ocaml_toplevel

let%expect_test "module-dependent function" =
  Ocaml_toplevel.eval
    ~code:
      {|
module type Id = sig
  type t

  val id : t -> t
end

let id (module A : Id) (x : A.t) = A.id x ;;
|};
  (* @mdexp.snapshot *)
  [%expect
    {|
    ```ocaml
    module type Id = sig
      type t

      val id : t -> t
    end

    let id (module A : Id) (x : A.t) = A.id x ;;
    ```

    ```ansi
    [1mLine 7, characters 23-32[0m:
    7 | let id (module A : Id) (x : A.t) = A.id x ;;
                               [1;31m^^^^^^^^^[0m
    [1;31mError[0m: This pattern matches values of type [1mA.t[0m
           but a pattern was expected which matches values of type [1m'a[0m
           The type constructor [1mA.t[0m would escape its scope
    ```
    |}];
  ()
;;

(* @mdexp

   As you can see above, constructs of these kinds are currently not in the
   language, but they are introduced in the version `5.5` of OCaml. We'll make
   sure to update that part of the doc when we migrate to these new features!

   Back to our tutorial: we titled it *provider-explicit* in reference to this.
   In the pattern we present here, functions take an additional *provider*
   argument to achieve a similar type-class style parametrization.

   In a nutshell:

   @mdexp.code *)

module type Id = sig
  type t

  val id : t -> t
end

type id = [ `Id ]

module Id : sig
  val t : ('a, (module Id with type t = 'a), [> id ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 'a module_type = (module Id with type t = 'a)
  end)

let id : type a. (a, [> id ]) Provider.t -> a -> a =
  fun provider x ->
  let module M = (val Provider.lookup provider ~trait:Id.t) in
  M.id x
;;

(* @mdexp.end *)

let _ = (id : (_, [> id ]) Provider.t -> _ -> _)

(* @mdexp

   In the rest of the tutorial, we cover this pattern in greater details and
   demonstrate how to use it with providers that implement multiple traits. We also
   provide examples where the Trait type is parametrized.

   Let's jump in!

   ## Functional providers

   In the [getting-started](../getting-started/) tutorial, we explored a scenario
   where providers were bundled with the value on which the Traits operate. Whether
   the functions exported by the Traits interfaces mutate the `t` value or not,
   this approach closely resembles how objects work in Object-Oriented languages.

   In contrast, this tutorial focuses on manipulating *providers* directly, without
   bundling them with values. This allows us to work with Traits that contain
   purely functional functions.

   ### Defining Traits

   Imagine we start with the following Trait:

   @mdexp.code *)

module type Doublable = sig
  type t

  val double : t -> t
end

(* @mdexp

   We define the expected *Provider* machinery, including a `Provider.Trait` for
   it:

   @mdexp.code *)

type doublable = [ `Doublable ]

module Doublable : sig
  val t : ('a, (module Doublable with type t = 'a), [> doublable ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 'a module_type = (module Doublable with type t = 'a)
  end)

(* @mdexp

   ### Writing Parametrized Code

   With no dependencies on actual providers, we can define functionality depending
   on the Trait interface only. This may look like this:

   @mdexp.code *)

let quadruple : type a. (a, [> doublable ]) Provider.t -> a -> a =
  fun provider t ->
  let module M = (val Provider.lookup provider ~trait:Doublable.t) in
  M.double (M.double t)
;;

(* @mdexp

   ### Implementing Providers

   Somewhere else, imagine we have modules implementing the expected signature:

   @mdexp.code *)

module Doublable_int = struct
  type t = int

  let double x = x * 2
end

module Doublable_float = struct
  type t = float

  let double x = x *. 2.
end

(* @mdexp

   We build *providers* values for these modules:

   @mdexp.code *)

let doublable_int () : (int, [> doublable ]) Provider.t =
  Provider.make [ Provider.implement Doublable.t ~impl:(module Doublable_int) ]
;;

let doublable_float () : (float, [> doublable ]) Provider.t =
  Provider.make [ Provider.implement Doublable.t ~impl:(module Doublable_float) ]
;;

(* @mdexp

   ### Instantiation

   And now, it is time to instantiate!

   @mdexp.code *)

let%expect_test "quadruple" =
  Printf.printf "%d\n" (quadruple (doublable_int ()) 1);
  [%expect {| 4 |}];
  Printf.printf "%.1f\n" (quadruple (doublable_float ()) 2.1);
  [%expect {| 8.4 |}];
  ()
;;

(* @mdexp

   ## Multiple Traits

   Let's define another Trait and write some code that require both Traits.

   ### Defining Traits

   @mdexp.code *)

module type Repeatable = sig
  type t

  val repeat : t -> t
end

(* @mdexp.end *)

(* @mdexp

   @mdexp.code *)

type repeatable = [ `Repeatable ]

module Repeatable : sig
  val t : ('a, (module Repeatable with type t = 'a), [> repeatable ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 'a module_type = (module Repeatable with type t = 'a)
  end)

(* @mdexp

   ### Writing Parametrized Code

   The function below requires both `repeatable` and `doublable` Traits:

   @mdexp.code *)

let double_then_repeat : type a. (a, [> doublable | repeatable ]) Provider.t -> a -> a =
  fun provider t ->
  let module D = (val Provider.lookup provider ~trait:Doublable.t) in
  let module R = (val Provider.lookup provider ~trait:Repeatable.t) in
  t |> D.double |> R.repeat
;;

(* @mdexp

   ### Implementing Providers

   Let's create a module working on int that implements both Traits:

   @mdexp.code *)

module Versatile_int = struct
  type t = int

  let double x = x * 2
  let repeat x = int_of_string (string_of_int x ^ string_of_int x)
end

(* @mdexp

   We can now build a *provider* for it:

   @mdexp.code *)

let versatile_int () : (int, [> doublable | repeatable ]) Provider.t =
  Provider.make
    [ Provider.implement Doublable.t ~impl:(module Versatile_int)
    ; Provider.implement Repeatable.t ~impl:(module Versatile_int)
    ]
;;

(* @mdexp

   The careful reader will note that this section requires careful handling, as
   there is no compiler assistance here. When defining providers, you must tag them
   correctly, or you may not be able to supply them to the functions you want, some
   traits may not be found at runtime, etc.

   ### Instantiation

   And now, time to instantiate!

   @mdexp.code *)

let%expect_test "double_then_repeat" =
  Printf.printf "%d\n" (double_then_repeat (versatile_int ()) 21);
  [%expect {| 4242 |}];
  ()
;;

(* @mdexp

   ## Parametrized types

   In this part, we'll demonstrate how to write code that is parametrized by an
   interface working on a parametrized type, a concept known as *higher-kinded
   polymorphism*.

   Consider values that can be mapped:

   ```ocaml
   module type Mappable = sig
     type 'a t

     val map : ('a -> 'b) -> 'a t -> 'b t
   end
   ```

   Imagine you want to write a function that applies the same mapping function
   multiple times for some reason.

   This kind of higher-kinded polymorphism will be achievable using modular
   explicit. It might look something like this in the future:

   ```ocaml
   let map_n_times (type a) (module A : Mappable) (x : a A.t) ~(f : a -> a) ~n =
     let rec loop n x = if n = 0 then x else loop (n - 1) (A.map f x) in
     loop n x
   ;;
   val map_n_times : (module A : Mappable) -> 'a A.t -> f:('a -> 'a) -> n:int -> 'a = <fun>
   ```

   In this section we show how to do this with the *provider* library, leveraging
   the [higher_kinded](https://github.com/janestreet/higher_kinded) library.

   ### Defining Traits

   We add the *Higher_kinded* machinery to our Trait signature, like so:

   @mdexp.code *)

module type Mappable = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  type higher_kinded

  val inject : 'a t -> ('a -> higher_kinded) Higher_kinded.t
  val project : ('a -> higher_kinded) Higher_kinded.t -> 'a t
end

(* @mdexp

   We define a provider trait for this interface:

   @mdexp.code *)

type mappable = [ `Mappable ]

(* @mdexp

   Note, you cannot write this (the `'a 't` syntax doesn't mean anything):

   ```ocaml
   module Mappable : sig
     val t : ('a 't, (module Mappable with type 'a t = 'a 't), [> mappable ]) Provider.Trait.t
   end = Provider.Trait.Create (struct
     type 'a 't module_type =  (module Mappable with type 'a t = 'a 't)
   end)
   ```

   ```
   Line 2, characters 17-18:
   Error: Syntax error
   ```

   This is where `Higher_kinded` comes to the rescue:

   @mdexp.code *)

module Mappable : sig
  val t
    : ( ('a -> 'higher_kinded) Higher_kinded.t
        , (module Mappable with type higher_kinded = 'higher_kinded)
        , [> mappable ] )
        Provider.Trait.t
end = Provider.Trait.Create1 (struct
    type (!'higher_kinded, 'a) t = ('a -> 'higher_kinded) Higher_kinded.t

    type 'higher_kinded module_type =
      (module Mappable with type higher_kinded = 'higher_kinded)
  end)

(* @mdexp

   ### Writing Parametrized Code

   That's it, we are on our way to write higher-kinded polymorphic functions:

   @mdexp.code *)

let map_n_times
  : type a t.
    ((a -> t) Higher_kinded.t, [> mappable ]) Provider.t
    -> (a -> t) Higher_kinded.t
    -> int
    -> f:(a -> a)
    -> (a -> t) Higher_kinded.t
  =
  fun provider t n ~f ->
  let module M = (val Provider.lookup provider ~trait:Mappable.t) in
  let at = M.project t in
  let rec loop n at = if n = 0 then at else loop (n - 1) (M.map f at) in
  M.inject (loop n at)
;;

(* @mdexp

   Granted, writing the type is quite a journey :-). But the implementation looks
   clear enough, doesn't it?

   ### Implementing Providers

   To make it work with higher-kinded types, we'll invoke the functor
   `Higher_kinded.Make` to create ready-to-use modules with the expected `Mappable`
   signature:

   @mdexp.code *)

module Higher_kinded_list = struct
  include List
  include Higher_kinded.Make (List)
end

module Higher_kinded_array = struct
  include Array
  include Higher_kinded.Make (Array)
end

(* @mdexp

   We can verify that the modules indeed implement the expected signatures:

   @mdexp.code *)

module _ : Mappable with type 'a t = 'a list = Higher_kinded_list
module _ : Mappable with type 'a t = 'a array = Higher_kinded_array

(* @mdexp

   We build *providers* values for these modules:

   @mdexp.code *)

let mappable_list ()
  : (('a -> Higher_kinded_list.higher_kinded) Higher_kinded.t, [> mappable ]) Provider.t
  =
  Provider.make [ Provider.implement Mappable.t ~impl:(module Higher_kinded_list) ]
;;

let mappable_array ()
  : (('a -> Higher_kinded_array.higher_kinded) Higher_kinded.t, [> mappable ]) Provider.t
  =
  Provider.make [ Provider.implement Mappable.t ~impl:(module Higher_kinded_array) ]
;;

(* @mdexp

   ### Instantiation

   And, again, time to instantiate our polymorphic code!

   @mdexp.code *)

let%expect_test "map_n_times" =
  map_n_times
    (mappable_list ())
    (List.init 10 Fun.id |> Higher_kinded_list.inject)
    3
    ~f:(fun x -> x + 1)
  |> Higher_kinded_list.project
  |> List.iter (fun x -> Printf.printf "%d " x);
  [%expect {| 3 4 5 6 7 8 9 10 11 12 |}];
  map_n_times
    (mappable_array ())
    ([| "a"; "b" |] |> Higher_kinded_array.inject)
    4
    ~f:(fun x -> x ^ x)
  |> Higher_kinded_array.project
  |> Array.iter (fun x -> Printf.printf "%s " x);
  [%expect {| aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbb |}];
  ()
;;

(* @mdexp

   ## Conclusion

   In this tutorial, we've demonstrated examples using the provider library that go
   beyond typical object-oriented patterns. We've shown how to write code
   parametrized by providers and how to make this work with purely functional
   functions, as well as with parametrized types.

   These techniques should offer convenient ways to parametrize code depending on
   various needs, and we hope they'll find practical applications in your favorite
   projects!

   We'll be keeping an eye on modular explicit too, and we're excited about the
   future of the module language! *)

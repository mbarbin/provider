# Handler Explicit

In this tutorial, we draw a parallel between a way to use the provider library, and [modular explicit](https://gallium.inria.fr/~remy/ocamod/modular-explicits.pdf).

## Introduction

Modular-explicit allows *module-dependent* functions to take a module implementing a Trait signature as an argument and use a type from the module to annotate subsequent arguments. For example:

<!-- $MDX skip -->
```ocaml
module type Id = sig type t val id : t -> t end

let id (module A : Id) (x : A.t) = A.id x
```

We called this tutorial *handler-explicit* in reference to this. In the pattern we present here, functions take an additional *handler* argument to achieve a similar type-class style parametrization.

In a nutshell:

```ocaml
module type Id = sig type t val id : t -> t end

type id = [ `Id ]

type (_, _, _) Provider.Trait.t +=
  | Id : ('a, (module Id with type t = 'a), [> id]) Provider.Trait.t

let id : type a. (a, [> id]) Provider.Handler.t -> a -> a =
  fun handler x ->
  let module M = (val Provider.Handler.lookup handler ~trait:Id) in
  M.id x
;;
```

In the rest of the tutorial, we cover this pattern in greater details and demonstrate how to use it with handlers that implement multiple traits. We also provide examples where the Trait type is parametrized.

Let's jump in!

## Functional handlers

In the [getting-started](../getting-started/) tutorial, we explored a scenario where handlers were bundled with the value on which the Traits operate. Whether the functions exported by the Traits interfaces mutate the `t` value or not, this approach closely resembles how objects work in Object-Oriented languages.

In contrast, this tutorial focuses on manipulating *handlers* directly, without bundling them with values. This allows us to work with Traits that contain purely functional functions.

### Defining Traits

Imagine we start with the following Trait:

```ocaml
module type Doublable = sig
  type t

  val double : t -> t
end
```

We define the expected *Provider* machinery, including a `Provider.Trait` for it:

```ocaml
type doublable = [ `Doublable ]

type (_, _, _) Provider.Trait.t +=
  | Doublable : ('a, (module Doublable with type t = 'a), [> doublable ]) Provider.Trait.t
```

### Writing Parametrized Code

With no dependencies on actual providers, we can define functionality depending on the Trait interface only. This may look like this:

```ocaml
# let quadruple : type a. (a, [> doublable ]) Provider.Handler.t -> a -> a =
  fun handler t ->
  let module M = (val Provider.Handler.lookup handler ~trait:Doublable) in
  M.double (M.double t)
val quadruple : ('a, [> doublable ]) Provider.Handler.t -> 'a -> 'a = <fun>
```

### Implementing Providers

Somewhere else, imagine we have modules implementing the expected signature:

```ocaml
module Doublable_int = struct
  type t = int

  let double x = x * 2
end

module Doublable_float = struct
  type t = float

  let double x = x *. 2.
end
```

We build *handlers* values for these modules:

```ocaml
# let doublable_int () : (int, [> doublable ]) Provider.Handler.t =
  Provider.Handler.make
    [ Provider.Trait.implement Doublable ~impl:(module Doublable_int) ]
val doublable_int : unit -> (int, [> doublable ]) Provider.Handler.t = <fun>

# let doublable_float () : (float, [> doublable ]) Provider.Handler.t =
  Provider.Handler.make
    [ Provider.Trait.implement Doublable ~impl:(module Doublable_float) ]
val doublable_float : unit -> (float, [> doublable ]) Provider.Handler.t =
  <fun>
```

### Instantiation

And now, it is time to instantiate!

```ocaml
# quadruple (doublable_int ()) 1
- : int = 4

# quadruple (doublable_float ()) 2.1
- : float = 8.4
```

## Multiple Traits

Let's define another Trait and write some code that require both Traits.

### Defining Traits

```ocaml
module type Repeatable = sig
  type t

  val repeat : t -> t
end
```

```ocaml
type repeatable = [ `Repeatable ]

type (_, _, _) Provider.Trait.t +=
  | Repeatable : ('a, (module Repeatable with type t = 'a), [> repeatable ]) Provider.Trait.t
```

### Writing Parametrized Code

The function below requires both `repeatable` and `doublable` Traits:

```ocaml
# let double_then_repeat : type a. (a, [> doublable | repeatable ]) Provider.Handler.t -> a -> a =
  fun handler t ->
  let module D = (val Provider.Handler.lookup handler ~trait:Doublable) in
  let module R = (val Provider.Handler.lookup handler ~trait:Repeatable) in
  R.repeat (D.double t)
val double_then_repeat :
  ('a, [> `Doublable | `Repeatable ]) Provider.Handler.t -> 'a -> 'a = <fun>
```

### Implementing Providers

Let's create a module working on int that implements both Traits:

```ocaml
module Versatile_int = struct
  type t = int

  let double x = x * 2

  let repeat x = int_of_string (string_of_int x ^ string_of_int x)
end
```

We can now build a *handler* for it:

```ocaml
# let versatile_int () : (int, [> doublable | repeatable ]) Provider.Handler.t =
  Provider.Handler.make
    [ Provider.Trait.implement Doublable ~impl:(module Versatile_int)
    ; Provider.Trait.implement Repeatable ~impl:(module Versatile_int)
    ]
val versatile_int :
  unit -> (int, [> `Doublable | `Repeatable ]) Provider.Handler.t = <fun>
```

The careful reader will note that this section requires careful handling, as there is no compiler assistance here. When defining handlers, you must tag them correctly, or you may not be able to supply them to the functions you want, some traits may not be found at runtime, etc.

### Instantiation

And now, time to instantiate!

```ocaml
# double_then_repeat (versatile_int ()) 21
- : int = 4242
```

## Parametrized types

In this part, we'll demonstrate how to write code that is parametrized by an interface working on a parametrized type, a concept known as *higher-kinded polymorphism*.

Consider values that can be mapped:

```ocaml
module type Mappable = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end
```

Imagine you want to write a function that applies the same mapping function multiple times for some reason.

This kind of higher-kinded polymorphism will be achievable using modular explicit. It might look something like this in the future:

<!-- $MDX skip -->
```ocaml
let map_n_times (type a) (module A : Mappable) (x : a A.t) ~(f : a -> a) ~n =
  let rec loop n x = if n = 0 then x else loop (n - 1) (A.map f x) in
  loop n x
;;
val map_n_times : (module A : Mappable) -> 'a A.t -> f:('a -> 'a) -> n:int -> 'a = <fun>
```

In this section we show how to do this with the *provider* library, leveraging the [higher_kinded](https://github.com/janestreet/higher_kinded) library.

### Defining Traits

We add the *Higher_kinded* machinery to our Trait signature, like so:

```ocaml
module type Mappable = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  type higher_kinded

  val inject : 'a t -> ('a -> higher_kinded) Higher_kinded.t
  val project : ('a -> higher_kinded) Higher_kinded.t -> 'a t
end
```

We define a provider trait for this interface:

```ocaml
type mappable = [ `Mappable ]
```

Note, you cannot write this (the `'a 't` syntax doesn't mean anything):

```ocaml
type (_, _, _) Provider.Trait.t +=
  | Mappable : ('a 't, (module Mappable with type 'a t = 'a 't), [> mappable ]) Provider.Trait.t
```
```mdx-error
Line 2, characters 22-23:
Error: Syntax error
```

This is where `Higher_kinded` comes to the rescue:

```ocaml
type (_, _, _) Provider.Trait.t +=
  | Mappable :
      ( ('a -> 'higher_kinded) Higher_kinded.t
        , (module Mappable with type higher_kinded = 'higher_kinded)
        , [> mappable ] )
        Provider.Trait.t
```

### Writing Parametrized Code

That's it, we are on our way to write higher-kinded polymorphic functions:

```ocaml
let map_n_times
  : type a t.
    ((a -> t) Higher_kinded.t, [> mappable ]) Provider.Handler.t
    -> (a -> t) Higher_kinded.t
    -> int
    -> f:(a -> a)
    -> (a -> t) Higher_kinded.t
  =
  fun handler t n ~f ->
  let module M = (val Provider.Handler.lookup handler ~trait:Mappable) in
  let at = M.project t in
  let rec loop n at = if n = 0 then at else loop (n - 1) (M.map f at) in
  M.inject (loop n at)
;;
```

Granted, writing the type is quite a journey :-). But the implementation looks clear enough, doesn't it?

### Implementing Providers

To make it work with higher-kinded types, we'll invoke the functor `Higher_kinded.Make` to create ready-to-use modules with the expected `Mappable` signature:

```ocaml
module Higher_kinded_list = struct
  include List
  include Higher_kinded.Make (List)
end

module Higher_kinded_array = struct
  include Array
  include Higher_kinded.Make (Array)
end
```

We can verify that the modules indeed implement the expected signatures:

```ocaml
module _ = (Higher_kinded_list : Mappable with type 'a t = 'a list)
module _ = (Higher_kinded_array : Mappable with type 'a t = 'a array)
```

We build *handlers* values for these modules:

```ocaml
# let mappable_list ()
  : ( ('a -> Higher_kinded_list.higher_kinded) Higher_kinded.t
      , [> mappable ] )
      Provider.Handler.t
  =
  Provider.Handler.make
    [ Provider.Trait.implement Mappable ~impl:(module Higher_kinded_list) ]
val mappable_list :
  unit ->
  (('a -> Higher_kinded_list.higher_kinded) Higher_kinded.t, [> mappable ])
  Provider.Handler.t = <fun>

# let mappable_array ()
  : ( ('a -> Higher_kinded_array.higher_kinded) Higher_kinded.t
      , [> mappable ] )
      Provider.Handler.t
  =
  Provider.Handler.make
    [ Provider.Trait.implement Mappable ~impl:(module Higher_kinded_array) ]
val mappable_array :
  unit ->
  (('a -> Higher_kinded_array.higher_kinded) Higher_kinded.t, [> mappable ])
  Provider.Handler.t = <fun>
```

### Instantiation

And, again, time to instantiate our polymorphic code!

```ocaml
# map_n_times
    (mappable_list ())
    (List.init 10 Fun.id |> Higher_kinded_list.inject)
    3
    ~f:(fun x -> x + 1)
  |> Higher_kinded_list.project
- : int list = List.(::) (3, [4; 5; 6; 7; 8; 9; 10; 11; 12])

# map_n_times
    (mappable_array ())
    ([| "a" ; "b" |] |> Higher_kinded_array.inject)
    4
    ~f:(fun x -> x ^ x)
  |> Higher_kinded_array.project
- : string array = [|"aaaaaaaaaaaaaaaa"; "bbbbbbbbbbbbbbbb"|]
```

## Conclusion

In this tutorial, we've demonstrated examples using the provider library that go beyond typical object-oriented patterns. We've shown how to write code parametrized by handlers and how to make this work with purely functional functions, as well as with parametrized types.

These techniques should offer convenient ways to parametrize code depending on various needs, and we hope they'll find practical applications in your favorite projects!

We'll be keeping an eye on modular explicit too, and we're excited about the future of the module language!

# Test magic

This test monitors an example that caused an earlier version of the library to
segfault. We keep it as regression test.

```ocaml
type ('a, 'impl, 'tag) Provider.Trait.t += Trait : (unit, 'a, [ `A ]) Provider.Trait.t
;;
```

```ansi
[1mLine 1, characters 0-86[0m:
1 | type ('a, 'impl, 'tag) Provider.Trait.t += Trait : (unit, 'a, [ `A ]) Provider.Trait.t
    [1;31m^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^[0m
[1;31mError[0m: Type definition [1mProvider.Trait.t[0m is not extensible
```

## Trying through the Create functors

The error above indicates that it is no longer possible to define the trait
that way, because we no longer expose any extensible variant to extend.
However, can a similar example be built through one of the functors? The short
answer is No. Below are a few attempts.

### Direct translation of the previous example

This is rejected through injectivity check.

```ocaml
module Trait = Provider.Trait.Create1 (struct
  type (_, _) t = unit
  type 'a module_type = 'a
end)
;;
```

```ansi
[1mLines 1-4, characters 15-4[0m:
1 | ...............Provider.Trait.Create1 (struct
2 |   type (_, _) t = unit
3 |   type 'a module_type = 'a
4 | end)
[1;31mError[0m: Modules do not match:
       sig type (_, _) t = unit type 'a module_type = 'a end
     is not included in sig type (!'a, 'b) t type 'a module_type end
     Type declarations do not match:
       type (_, _) t = unit
     is not included in
       type (!'a, 'b) t
     Their variances do not agree.
```

Trying to force the injectivity won't do either.

```ocaml
module Trait = Provider.Trait.Create1 (struct
  type (!'a, _) t = unit
  type 'a module_type = 'a
end)
;;
```

```ansi
[1mLine 2, characters 2-24[0m:
2 |   type (!'a, _) t = unit
      [1;31m^^^^^^^^^^^^^^^^^^^^^^[0m
[1;31mError[0m: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
```

### Tweaking the original example somehow

Replacing `unit` by a record or a variant doesn't make the injectivity
annotation valid.

```ocaml
type record = { a : string }

module Trait = Provider.Trait.Create1 (struct
  type (!'a, _) t = record
  type 'a module_type = 'a
end)
;;
```

```ansi
[1mLine 4, characters 2-26[0m:
4 |   type (!'a, _) t = record
      [1;31m^^^^^^^^^^^^^^^^^^^^^^^^[0m
[1;31mError[0m: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
```

```ocaml
type variant = A

module Trait = Provider.Trait.Create1 (struct
  type (!'a, _) t = variant
  type 'a module_type = 'a
end)
;;
```

```ansi
[1mLine 4, characters 2-27[0m:
4 |   type (!'a, _) t = variant
      [1;31m^^^^^^^^^^^^^^^^^^^^^^^^^[0m
[1;31mError[0m: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
```

If you bind the `'a` parameter so the annotation pass, the definition of the
trait is valid.

```ocaml
module Trait = Provider.Trait.Create1 (struct
    type (!'a, _) t = 'a
    type 'a module_type = 'a
  end)
```

Granted, you can coerce it with different types:

```ocaml
let _a = (Trait.t : ('a, 'a, [ `A ]) Provider.Trait.t)
let _b = (Trait.t : (string, string, [ `A ]) Provider.Trait.t)
let _c = (Trait.t : (int, int, [ `A ]) Provider.Trait.t)
```

But the point is that now the parameters of the first and second arguments are
bound:

```ocaml
module Trait = Provider.Trait.Create1 (struct
  type (!'a, _) t = 'a
  type 'a module_type = 'a
end)

let _ = (Trait.t : (unit, string, [ `A]) Provider.Trait.t) ;;
```

```ansi
[1mLine 6, characters 9-16[0m:
6 | let _ = (Trait.t : (unit, string, [ `A]) Provider.Trait.t) ;;
             [1;31m^^^^^^^[0m
[1;31mError[0m: The value [1mTrait.t[0m has type
         [1m(unit, unit, 'a) Provider.Trait.t[0m =
           [1m(unit, unit, 'a) Provider__Trait0.t[0m
       but an expression was expected of type
         [1m(unit, string, [ `A ]) Provider.Trait.t[0m =
           [1m(unit, string, [ `A ]) Provider__Trait0.t[0m
       Type [1munit[0m is not compatible with type [1mstring[0m
```

So, the rest of the test does not apply.

```ocaml
let _a = (Trait.t : (string, string, [ `A ]) Provider.Trait.t)
let _h = Provider.make [ Provider.implement _a ~impl:"hello" ]
let _b = (Trait.t : (int, int, [ `A ]) Provider.Trait.t)
```

```ocaml
module Trait = Provider.Trait.Create1 (struct
  type (!'a, _) t = 'a
  type 'a module_type = 'a
end)

let a = (Trait.t : (string, string, [ `A ]) Provider.Trait.t)
let h = Provider.make [ Provider.implement a ~impl:"hello" ]
let b = (Trait.t : (int, int, [ `A ]) Provider.Trait.t)

let crash () =
  let (i : int) = Provider.lookup h ~trait:b in
  assert (Obj.is_int (Obj.repr i))
;;
```

```ansi
[1mLine 11, characters 43-44[0m:
11 |   let (i : int) = Provider.lookup h ~trait:b in
                                                [1;31m^[0m
[1;31mError[0m: The value [1mb[0m has type
         [1m(int, int, [ `A ]) Provider.Trait.t[0m =
           [1m(int, int, [ `A ]) Provider__Trait0.t[0m
       but an expression was expected of type
         [1m(string, 'a, 'b) Provider.Trait.t[0m =
           [1m(string, 'a, 'b) Provider__Trait0.t[0m
       Type [1mint[0m is not compatible with type [1mstring[0m
```

## For reference

We're keeping the rest of the original test for reference only, it cannot be
written with recent versions of the library anymore.

```ocaml
let a = (Trait : (unit, string, [ `A ]) Provider.Trait.t)
let h = Provider.make [ Provider.implement a ~impl:"hello" ]
let b = (Trait : (unit, int, [ `A ]) Provider.Trait.t)

let%expect_test "crash" =
  let (i : int) = Provider.lookup h ~trait:b in
  print_dyn (Dyn.record [ "is_int", Dyn.bool (Obj.is_int (Obj.repr i)) ]);
  [%expect {| { is_int = false } |}];
  ()
;;
```

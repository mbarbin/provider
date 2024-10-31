# Test magic

This test monitors an example that caused an earlier version of the library to segfault. We keep it as regression test.

```ocaml
type ('a, 'impl, 'tag) Provider.Trait.t += Trait : (unit, 'a, [ `A ]) Provider.Trait.t
```
```mdx-error
Line 1, characters 1-87:
Error: Type definition Provider.Trait.t is not extensible
```

## Trying through the Create functors

The error above indicates that it is no longer possible to define the trait that way, because we no longer expose any extensible variant to extend. However, can a similar example be built through one of the functors? The short answer is No. Below are a few attempts.

### Direct translation of the previous example

This is rejected through injectivity check.

```ocaml
module Trait = Provider.Trait.Create1 (struct
  type 'a t = unit
  type 'a module_type = 'a
end)
```
```mdx-error
Lines 1-4, characters 16-7:
Error: Modules do not match:
       sig type 'a t = unit type 'a module_type = 'a end
     is not included in sig type !'a t type 'a module_type end
     Type declarations do not match:
       type 'a t = unit
     is not included in
       type !'a t
     Their variances do not agree.
     File "src/provider.mli", line 49, characters 6-16: Expected declaration
```

Trying to force the injectivity won't do either.

```ocaml
module Trait = Provider.Trait.Create1 (struct
  type !'a t = unit
  type 'a module_type = 'a
end)
```
```mdx-error
Line 2, characters 5-22:
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
```

### Tweaking the original example somehow

Replacing `unit` by a record or a variant doesn't make the injectivity annotation valid.

```ocaml
type record = { a : string }

module Trait = Provider.Trait.Create1 (struct
  type !'a t = record
  type 'a module_type = 'a
end)
```
```mdx-error
Line 4, characters 5-24:
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
```

```ocaml
type variant = A

module Trait = Provider.Trait.Create1 (struct
  type !'a t = variant
  type 'a module_type = 'a
end)
```
```mdx-error
Line 4, characters 5-25:
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be injective invariant,
       but it is unrestricted.
```

If you bind the `'a` parameter so the annotation pass, the definition of the trait is valid.

```ocaml
module Trait = Provider.Trait.Create1 (struct
  type !'a t = 'a
  type 'a module_type = 'a
end)
```

Granted, you can coerce it with different types:

```ocaml
let a = (Trait.t : ('a, 'a, [ `A]) Provider.Trait.t) ;;
let b = (Trait.t : (string, string, [ `A]) Provider.Trait.t) ;;
let c = (Trait.t : (int, int, [ `A]) Provider.Trait.t) ;;
```

But the point is that now the parameters of the first and second arguments are bound:

```ocaml
let _ = (Trait.t : (unit, string, [ `A]) Provider.Trait.t) ;;
```
```mdx-error
Line 1, characters 10-17:
Error: This expression has type (unit, unit, 'a) Provider.Trait.t
       but an expression was expected of type
         (unit, string, [ `A ]) Provider.Trait.t
       Type unit is not compatible with type string
```

So, the rest of the test does not apply.

```ocaml
let a = (Trait.t : (string, string, [ `A ]) Provider.Trait.t)
let h = Provider.Handler.make [ Provider.Trait.implement a ~impl:"hello" ]
let b = (Trait.t : (int, int, [ `A ]) Provider.Trait.t)
```

```ocaml
let crash () =
  let (i : int) = Provider.Handler.lookup h ~trait:b in
  assert (Stdlib.Obj.is_int (Stdlib.Obj.repr i))
;;
```
```mdx-error
Line 2, characters 54-55:
Error: This expression has type (int, int, [ `A ]) Provider.Trait.t
       but an expression was expected of type
         (string, 'a, 'b) Provider.Trait.t
       Type int is not compatible with type string
```

## For reference

We're keeping the rest of the original test for reference only, it cannot be written with recent versions of the library anymore.

<!-- $MDX skip -->
```ocaml
let a = (Trait : (unit, string, [ `A ]) Provider.Trait.t)
let h = Provider.Handler.make [ Provider.Trait.implement a ~impl:"hello" ]
let b = (Trait : (unit, int, [ `A ]) Provider.Trait.t)

let%expect_test "crash" =
  let (i : int) = Provider.Handler.lookup h ~trait:b in
  print_s [%sexp { is_int = (Stdlib.Obj.is_int (Stdlib.Obj.repr i) : bool) }];
  [%expect {| ((is_int false)) |}];
  ()
;;
```
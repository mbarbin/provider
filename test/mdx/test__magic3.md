# Test magic

This test monitors an example that caused an earlier version of the library to segfault. We keep it as regression test.

```ocaml
type ('a, 'impl, 'tag) Provider.Trait.t += Trait : (unit, 'a, [ `A ]) Provider.Trait.t
```

## For reference

We're keeping the rest of the test for reference only, it cannot be written with recent versions of the library anymore.

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

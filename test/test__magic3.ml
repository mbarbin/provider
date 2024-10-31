(* This test monitors an example that causes the current version of the library
   to segfault. We keep it as regression test. *)

type ('a, 'impl, 'tag) Provider.Trait.t += Trait : (unit, 'a, [ `A ]) Provider.Trait.t

let a = (Trait : (unit, string, [ `A ]) Provider.Trait.t)
let h = Provider.Handler.make [ Provider.Trait.implement a ~impl:"hello" ]
let b = (Trait : (unit, int, [ `A ]) Provider.Trait.t)

let%expect_test "crash" =
  let (i : int) = Provider.Handler.lookup h ~trait:b in
  print_s [%sexp { is_int = (Stdlib.Obj.is_int (Stdlib.Obj.repr i) : bool) }];
  [%expect {| ((is_int false)) |}];
  ()
;;

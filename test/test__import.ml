(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

module Import = Provider.Private.Import

let%expect_test "Array.for_alli" =
  let t = Array.init 10 ~f:Fn.id in
  require (Import.Array.for_alli t ~f:Int.equal);
  [%expect {||}];
  t.(9) <- 0;
  require (not (Import.Array.for_alli t ~f:Int.equal));
  [%expect {||}];
  ()
;;

let%expect_test "Ordering.of_int" =
  let test i (ordering : Import.Ordering.t) =
    require (Poly.equal (Import.Ordering.of_int i) ordering)
  in
  test 0 Equal;
  [%expect {||}];
  test 1 Greater;
  [%expect {||}];
  test (-1) Less;
  [%expect {||}];
  test (Stdlib.Int.compare 42 42) Equal;
  [%expect {||}];
  test (Stdlib.Char.compare 'Z' 'A') Greater;
  [%expect {||}];
  test (Stdlib.String.compare "42" "43") Less;
  [%expect {||}];
  ()
;;

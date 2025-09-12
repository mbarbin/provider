(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

module Import = Provider.Private.Import

let%expect_test "Array.for_alli" =
  let t = Array.init 10 ~f:Fn.id in
  require [%here] (Import.Array.for_alli t ~f:(fun i x -> i = x));
  [%expect {||}];
  t.(9) <- 0;
  require [%here] (not (Import.Array.for_alli t ~f:(fun i x -> i = x)));
  [%expect {||}];
  ()
;;

let%expect_test "Ordering.of_int" =
  let test i (ordering : Import.Ordering.t) =
    require [%here] (Poly.equal (Import.Ordering.of_int i) ordering)
  in
  test 0 Equal;
  [%expect {||}];
  test 1 Greater;
  [%expect {||}];
  test (-1) Less;
  [%expect {||}];
  test (Int.compare 42 42) Equal;
  [%expect {||}];
  test (Char.compare 'Z' 'A') Greater;
  [%expect {||}];
  test (String.compare "42" "43") Less;
  [%expect {||}];
  ()
;;

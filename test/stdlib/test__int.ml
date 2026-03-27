(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

let%expect_test "to_string_hum" =
  let test i = print_endline (Int.to_string_hum i) in
  test 0;
  [%expect {| 0 |}];
  test 999;
  [%expect {| 999 |}];
  test 1000;
  [%expect {| 1_000 |}];
  test 1234;
  [%expect {| 1_234 |}];
  test 12345;
  [%expect {| 12_345 |}];
  test 123456;
  [%expect {| 123_456 |}];
  test 1234567;
  [%expect {| 1_234_567 |}];
  test 123_456_789;
  [%expect {| 123_456_789 |}];
  test (-1);
  [%expect {| -1 |}];
  test (-999);
  [%expect {| -999 |}];
  test (-1000);
  [%expect {| -1_000 |}];
  test (-1234567);
  [%expect {| -1_234_567 |}];
  test (-123_456_789);
  [%expect {| -123_456_789 |}];
  ()
;;

let%expect_test "sexp_of_t" =
  let test i = print_s (Int.sexp_of_t i) in
  test 42;
  [%expect {| 42 |}];
  test 1234;
  [%expect {| 1_234 |}];
  test (-1234);
  [%expect {| -1_234 |}];
  ()
;;

let%expect_test "to_dyn" =
  let test i = print_dyn (Int.to_dyn i) in
  test 0;
  [%expect {| 0 |}];
  test 42;
  [%expect {| 42 |}];
  ()
;;

(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

let%expect_test "hd_exn" =
  print_dyn (Int.to_dyn (List.hd_exn [ 1; 2; 3 ]));
  [%expect {| 1 |}];
  require_does_raise (fun () -> List.hd_exn []);
  [%expect {| Failure("List.hd_exn") |}];
  ()
;;

let%expect_test "is_empty" =
  require (List.is_empty []);
  [%expect {| |}];
  require (not (List.is_empty [ 1 ]));
  [%expect {| |}];
  ()
;;

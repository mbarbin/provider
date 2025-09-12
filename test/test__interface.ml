(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

let%expect_test "dedup_sorted_keep_last" =
  let dedup = Provider.Private.dedup_sorted_keep_last in
  let test list =
    print_s
      [%sexp
        (dedup list ~compare:(fun (a, _) (b, _) -> Int.compare a b) : (int * string) list)]
  in
  test [];
  [%expect {| () |}];
  test [ 1, "a" ];
  [%expect {| ((1 a)) |}];
  test [ 1, "a"; 1, "b" ];
  [%expect {| ((1 b)) |}];
  test [ 1, "a"; 2, "b" ];
  [%expect
    {|
    ((1 a)
     (2 b))
    |}];
  test [ 1, "a"; 2, "b"; 3, "c"; 3, "c'"; 4, "d"; 4, "d'"; 5, "e" ];
  [%expect
    {|
    ((1 a)
     (2 b)
     (3 c')
     (4 d')
     (5 e))
    |}];
  ()
;;

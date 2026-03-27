(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

let protect ~f ~finally =
  match f () with
  | v ->
    finally ();
    v
  | exception e ->
    finally ();
    raise e
;;

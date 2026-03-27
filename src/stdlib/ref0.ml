(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

let set_temporarily r value ~f =
  let old = !r in
  r := value;
  Fun.protect ~finally:(fun () -> r := old) f
;;

(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

include Stdlib.StringLabels

let compare a b = Ordering0.of_int (Stdlib.String.compare a b)
let ends_with t ~suffix = Stdlib.String.ends_with ~suffix t

let[@tail_mod_cons] rec drop_last_empty = function
  | [] -> []
  | [ "" ] -> []
  | x :: rest ->
    (* Coverage is off in the second part of the expression because the
       instrumentation breaks [@tail_mod_cons], triggering warning 71. *)
    x :: (drop_last_empty rest [@coverage off])
;;

let split_lines t =
  let lines = Stdlib.String.split_on_char '\n' t in
  (* Remove trailing empty string, matching Base's behavior. *)
  drop_last_empty lines
;;

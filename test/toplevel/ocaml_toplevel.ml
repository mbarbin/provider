(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

module Unix = UnixLabels

let toplevel_exe = "./provider_toplevel.exe"

let string_is_substring s ~substring =
  let len_s = String.length s in
  let len_sub = String.length substring in
  let rec aux i =
    if i + len_sub > len_s
    then false
    else if String.equal (String.sub s ~pos:i ~len:len_sub) substring
    then true
    else aux (i + 1)
  in
  aux 0
;;

let truncate_after_line lines ~delimiter =
  let[@tail_mod_cons] rec aux = function
    | [] -> ([] [@coverage off])
    | line :: rest ->
      if string_is_substring line ~substring:delimiter
      then [ line ]
      else
        (* Coverage is off in the second part of the expression because the
           instrumentation breaks [@tail_mod_cons], triggering warning 71. *)
        line :: (aux rest [@coverage off])
  in
  aux lines
;;

let eval ?truncate_after code =
  let code = String.trim code in
  print_endline "```ocaml";
  print_endline code;
  print_endline "```";
  print_endline "";
  print_endline "```ansi";
  let cmd = toplevel_exe ^ " -noprompt -no-version -color always" in
  let ic, oc, ec = Unix.open_process_full cmd ~env:[||] in
  output_string oc code;
  output_char oc '\n';
  close_out oc;
  let stdout_content = In_channel.input_all ic in
  let stderr_content = In_channel.input_all ec in
  let status = Unix.close_process_full (ic, oc, ec) in
  let stdout_trimmed =
    String.trim stdout_content
    |> String.split_on_char ~sep:'\n'
    |> (match truncate_after with
      | None -> Fun.id
      | Some delimiter -> truncate_after_line ~delimiter)
    |> String.concat ~sep:"\n"
  in
  if String.length stdout_trimmed > 0 then print_endline stdout_trimmed;
  let stderr_trimmed = String.trim stderr_content in
  if String.length stderr_trimmed > 0 then print_endline stderr_trimmed [@coverage off];
  (match status with
   | WEXITED 0 -> ()
   | _ ->
     (match[@coverage off] status with
      | WEXITED n -> Printf.printf "[%d]\n" n
      | WSIGNALED n -> Printf.printf "[signal %d]\n" n
      | WSTOPPED n -> Printf.printf "[stopped %d]\n" n));
  print_endline "```"
;;

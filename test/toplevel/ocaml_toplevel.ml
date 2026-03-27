(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

module Unix = UnixLabels

let toplevel_exe = "./provider_toplevel.exe"

let file_path_re =
  lazy
    (Re.compile
       (Re.seq
          [ Re.str {|File "|}
          ; Re.group (Re.rep1 (Re.compl [ Re.char '"' ]))
          ; Re.str {|"|}
          ]))
;;

let use_basename_in_file_paths s =
  Re.replace (Lazy.force file_path_re) s ~f:(fun group ->
    let path = Re.Group.get group 1 in
    Printf.sprintf {|File "%s"|} (Filename.basename path))
;;

let eval ~code =
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
  let stdout_trimmed = String.trim stdout_content |> use_basename_in_file_paths in
  if String.length stdout_trimmed > 0 then print_endline stdout_trimmed;
  let stderr_trimmed = String.trim stderr_content |> use_basename_in_file_paths in
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

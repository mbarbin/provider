(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

module Unix = UnixLabels

let toplevel_exe = "./provider_toplevel.exe"

let is_toplevel_initialization_message line =
  let stripped = String.strip line in
  String.is_empty stripped
  || String.equal stripped "#"
  || List.exists
       ~f:(fun suffix -> String.is_suffix stripped ~suffix)
       [ ": added to search path"; ".cma: loaded"; ".cmo: loaded"; ".cmxs: loaded" ]
  || List.exists
       ~f:(fun prefix -> String.is_prefix stripped ~prefix)
       [ "Findlib has been successfully loaded"
       ; {|#require "package"|}
       ; "#list;;"
       ; "#camlp4o;;"
       ; "#camlp4r;;"
       ; "#predicates"
       ; "Topfind.reset"
       ; "#thread;;"
       ]
;;

let eval ~code =
  Stdlib.print_endline "```ocaml";
  Stdlib.print_endline code;
  Stdlib.print_endline "```";
  Stdlib.print_endline "";
  Stdlib.print_endline "```terminal";
  let cmd = toplevel_exe ^ " -noprompt -no-version -color always" in
  let ic, oc, ec = Unix.open_process_full cmd ~env:[||] in
  Stdlib.output_string oc code;
  Stdlib.close_out oc;
  let stdout_content = Stdlib.In_channel.input_all ic in
  let stderr_content = Stdlib.In_channel.input_all ec in
  let _status = Unix.close_process_full (ic, oc, ec) in
  let output = String.strip (stdout_content ^ stderr_content) in
  let lines = String.split_lines output in
  let rec skip_init = function
    | line :: rest when is_toplevel_initialization_message line -> skip_init rest
    | lines -> lines
  in
  List.iter ~f:Stdlib.print_endline (skip_init lines);
  Stdlib.print_endline "```"
;;

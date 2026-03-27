(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

include Stdlib.Int

let to_dyn t = Dyn.Int t
let hash (t : t) = Stdlib.Hashtbl.hash t
let incr = Stdlib.incr
let ( + ) = Stdlib.( + )
let zero = 0

let to_string_hum ?(delimiter = '_') t =
  let s = to_string t in
  let n = String.length s in
  let start = if n > 0 && s.[0] = '-' then 1 else 0 in
  let digits = n - start in
  if digits <= 3
  then s
  else (
    let groups = (digits + 2) / 3 in
    let len = n + groups - 1 in
    let buf = Bytes.create len in
    let first_group = digits - ((groups - 1) * 3) in
    Bytes.blit_string s 0 buf 0 start;
    Bytes.blit_string s start buf start first_group;
    let src_pos = ref (start + first_group) in
    let dst_pos = ref (start + first_group) in
    for _ = 2 to groups do
      Bytes.set buf !dst_pos delimiter;
      Stdlib.incr dst_pos;
      Bytes.blit_string s !src_pos buf !dst_pos 3;
      src_pos := !src_pos + 3;
      dst_pos := !dst_pos + 3
    done;
    Bytes.unsafe_to_string buf)
;;

let sexp_of_t t = Sexplib0.Sexp.Atom (to_string_hum t)

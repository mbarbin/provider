(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

include Stdlib.ArrayLabels

let map t ~f = Stdlib.Array.map f t
let iter t ~f = Stdlib.Array.iter f t
let init n ~f = Stdlib.Array.init n f

let find_map t ~f =
  let len = length t in
  let rec loop i =
    if i >= len
    then None
    else (
      match f (unsafe_get t i) with
      | Some _ as r -> r
      | None -> loop (i + 1))
  in
  loop 0
;;

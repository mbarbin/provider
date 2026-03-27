(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

include Stdlib.ListLabels

(* Re-bind labeled functions to place the list argument first, matching Base's
   convention [list ~f] rather than ListLabels' convention [~f list]. *)
let map l ~f = Stdlib.List.map f l
let mapi l ~f = Stdlib.List.mapi f l
let iter l ~f = Stdlib.List.iter f l
let iteri l ~f = Stdlib.List.iteri f l
let filter l ~f = Stdlib.List.filter f l
let filter_map l ~f = Stdlib.List.filter_map f l
let find_map l ~f = Stdlib.List.find_map f l
let concat_map l ~f = Stdlib.List.concat_map f l
let for_all l ~f = Stdlib.List.for_all f l
let exists l ~f = Stdlib.List.exists f l
let init n ~f = Stdlib.List.init n f
let sort l ~compare = Stdlib.List.sort compare l

module type Summable = sig
  type t

  val zero : t
  val ( + ) : t -> t -> t
end

let sum (type a) (module M : Summable with type t = a) l ~f =
  fold_left ~init:M.zero ~f:(fun acc x -> M.( + ) acc (f x)) l
;;

let hd_exn = function
  | x :: _ -> x
  | [] -> failwith "List.hd_exn"
;;

let is_empty = function
  | [] -> true
  | _ :: _ -> false
;;

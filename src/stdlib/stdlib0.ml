(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

module type T = sig
  type t
end

module Array = Array0
module Code_error = Code_error0
module Dyn = Dyn0
module Exn = Exn0
module Fn = Fn0
module Int = Int0
module List = List0
module Option = Option0
module Ordering = Ordering0
module Poly = Poly0
module Pp = Pp0
module Ref = Ref0
module Sexp = Sexplib0.Sexp
module String = String0
module With_equal_and_dyn = With_equal_and_dyn0

let ( = ) = `Use_Poly_when_needed
let phys_equal = Stdlib.( == )
let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)
let print_s sexp = Stdlib.print_endline (Sexp.to_string_hum sexp)
let print_endline = Stdlib.print_endline
let require cond = if not cond then failwith "Required condition does not hold"

let require_does_raise f =
  match f () with
  | _ -> Code_error.raise "Did not raise." []
  | exception e -> print_endline (Printexc.to_string e)
;;

let require_equal
      (type a)
      (module M : With_equal_and_dyn.S with type t = a)
      (v1 : a)
      (v2 : a)
  =
  if not (M.equal v1 v2)
  then
    Code_error.raise
      "Values are not equal."
      [ "v1", v1 |> M.to_dyn; "v2", v2 |> M.to_dyn ]
;;

let require_not_equal
      (type a)
      (module M : With_equal_and_dyn.S with type t = a)
      (v1 : a)
      (v2 : a)
  =
  if M.equal v1 v2
  then Code_error.raise "Values are equal." [ "v1", v1 |> M.to_dyn; "v2", v2 |> M.to_dyn ]
;;

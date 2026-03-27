(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

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

val phys_equal : 'a -> 'a -> bool
val print_dyn : Dyn.t -> unit
val print_s : Sexp.t -> unit

(** Expect test helpers. *)

val print_endline : string -> unit
val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit
val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
val require_not_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit

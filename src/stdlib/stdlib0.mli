(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

module Code_error = Code_error0
module Dyn = Dyn0
module Int = Int0
module Pp = Pp0
module With_equal_and_dyn = With_equal_and_dyn0

val print_dyn : Dyn.t -> unit
val print_s : Sexplib0.Sexp.t -> unit

(** Expect test helpers. *)

val print_endline : string -> unit
val require : bool -> unit
val require_does_raise : (unit -> 'a) -> unit
val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
val require_not_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit

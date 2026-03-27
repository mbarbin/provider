(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.Int
end

val to_dyn : t -> Dyn.t
val hash : t -> int
val incr : t ref -> unit
val ( + ) : t -> t -> t
val zero : t
val compare : t -> t -> Ordering0.t
val to_string_hum : ?delimiter:char -> t -> string
val sexp_of_t : t -> Sexplib0.Sexp.t

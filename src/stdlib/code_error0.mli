(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

(** A programming error that should be reported upstream. *)

type t =
  { message : string
  ; data : (string * Dyn.t) list
  }

exception E of t

val raise : string -> (string * Dyn.t) list -> _

(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.ListLabels
end

val map : 'a list -> f:('a -> 'b) -> 'b list
val iter : 'a list -> f:('a -> unit) -> unit
val filter : 'a list -> f:('a -> bool) -> 'a list
val init : int -> f:(int -> 'a) -> 'a list
val sort : 'a list -> compare:('a -> 'a -> Ordering0.t) -> 'a list

module type Summable = sig
  type t

  val zero : t
  val ( + ) : t -> t -> t
end

val sum : (module Summable with type t = 'a) -> 'b list -> f:('b -> 'a) -> 'a
val hd_exn : 'a list -> 'a
val is_empty : 'a list -> bool

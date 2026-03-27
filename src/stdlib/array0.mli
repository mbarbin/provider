(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.ArrayLabels
end

val map : 'a array -> f:('a -> 'b) -> 'b array
val iter : 'a array -> f:('a -> unit) -> unit
val init : int -> f:(int -> 'a) -> 'a array
val find_map : 'a array -> f:('a -> 'b option) -> 'b option

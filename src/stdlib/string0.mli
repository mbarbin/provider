(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.StringLabels
end

val compare : string -> string -> Ordering0.t
val ends_with : string -> suffix:string -> bool
val split_lines : string -> string list

(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

val phys_equal : 'a -> 'a -> bool

module Array : sig
  include module type of ArrayLabels

  val for_alli : 'a array -> f:(int -> 'a -> bool) -> bool
end

module Hashtbl : sig
  include module type of MoreLabels.Hashtbl
end

module Int : sig
  include module type of Int

  (** We re-export hash functions here to make the code compatible with earlier
      ocaml versions. [Stdlib.Int.hash] is available since [ocaml.5.1]. *)

  val hash : int -> int
  val seeded_hash : int -> int -> int
end

module List : sig
  include module type of ListLabels

  val stable_sort : 'a list -> compare:('a -> 'a -> int) -> 'a list
end

module Ordering : sig
  type t =
    | Less
    | Equal
    | Greater

  val of_int : int -> t
end

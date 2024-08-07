val phys_equal : 'a -> 'a -> bool

module Array : sig
  include module type of ArrayLabels

  val for_alli : 'a array -> f:(int -> 'a -> bool) -> bool
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

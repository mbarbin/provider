val phys_equal : 'a -> 'a -> bool

module Array : sig
  include module type of ArrayLabels

  val for_alli : 'a array -> f:(int -> 'a -> bool) -> bool
end

module List : sig
  include module type of ListLabels
end

module Ordering : sig
  type t =
    | Equal
    | Less
    | Greater

  val of_int : int -> t
end

let phys_equal = ( == )

module Array = struct
  include ArrayLabels

  let for_alli =
    let rec loop t ~f i =
      if i = length t then true else if f i t.(i) then loop t ~f (i + 1) else false
    in
    fun t ~f -> loop t ~f 0
  ;;
end

module List = ListLabels

module Ordering = struct
  type t =
    | Equal
    | Less
    | Greater

  let of_int i = if i < 0 then Less else if i = 0 then Equal else Greater
end

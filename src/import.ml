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

module Int = struct
  include Int

  let hash = (Hashtbl.hash : int -> int)
  let seeded_hash = (Hashtbl.seeded_hash : int -> int -> int)
end

module List = struct
  include ListLabels

  let stable_sort t ~compare = stable_sort t ~cmp:compare
end

module Ordering = struct
  type t =
    | Less
    | Equal
    | Greater

  let of_int i = if i < 0 then Less else if i = 0 then Equal else Greater
end

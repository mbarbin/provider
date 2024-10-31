type (_, _) t =
  | Equal : ('a, 'a) t
  | Not_equal : ('a, 'b) t

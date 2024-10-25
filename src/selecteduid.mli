type t = int
val sexp_of_t : t -> Sexp.t
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

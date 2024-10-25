type t = int

let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_int
let equal = Int.equal
let compare = Int.compare
let hash = Int.hash

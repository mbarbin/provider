type ('t, 'module_type, 'tag) t

module Create (X : sig
    type 'a module_type
  end) : sig
  val t : ('a, 'a X.module_type, _) t
end

module Create0 (X : sig
    type t
    type module_type
  end) : sig
  val t : (X.t, X.module_type, _) t
end

module Create1 (X : sig
    type !'a t
    type 'a module_type
  end) : sig
  val t : ('a X.t, 'a X.module_type, _) t
end

module Create2 (X : sig
    type (!'a, !'b) t
    type ('a, 'b) module_type
  end) : sig
  val t : (('a, 'b) X.t, ('a, 'b) X.module_type, _) t
end

(** Return a id that is unique to this trait for the lifetime of the program. *)
val uid : _ t -> int

val same_witness : ('t, 'mt1, _) t -> ('t, 'mt2, _) t -> ('mt1, 'mt2) Type_eq_opt.t

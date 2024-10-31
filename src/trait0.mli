type ('t, 'module_type, 'tag) t = ..

(** Through the use of this library, a tag is only valid if it has no arguments.
    This function is used to check that, and used to validate traits built by
    the user to detect invalid usage of the library. *)
val is_valid : _ t -> bool

(** Return a id that is unique to this trait for the lifetime of the program. *)
val uid : _ t -> int

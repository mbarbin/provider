(** A mutable data structure to attach names to trait uids.

    This is meant to be used in tests only to make the information attached to
    trait more meaningful. *)

(** A mutable data structure to store the names. *)
type t

val create : unit -> t

(** [default] is the one and only hashtbl used by the provider library. This is
    where info is stored when [Provider.Trait.Info.register_name] is called. *)
val default : t

val set_name : t -> _ Trait0.t -> name:string -> unit
val get_name : t -> _ Trait0.t -> string option

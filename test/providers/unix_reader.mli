(** [Unix_reader] is a provider for the {!module:Interface.Directory_reader}
    interface based on [Unix].

    It is meant to demonstrate how to illustrate how multiple providers may be
    implemented the same interfaces. {!module:Eio_reader} is another provider
    for the {!module:Interface.Directory_reader} interface.

    The structure of this file is very similar to the [Eio_reader] module, thus
    is not documented in details. Refer to {!Eio_reader} for more. *)

type t = unit

val make : t -> Interface.Directory_reader.tag Provider.t

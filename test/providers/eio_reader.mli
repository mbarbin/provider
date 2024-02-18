(** [Eio_reader] is a provider for the {!module:Directory_reader} and
    {!module:File_reader} interfaces, based on [Eio].

    It is a thin wrapper around [Eio.Path], meant to demonstrate how to
    implement a provider for a given interface. *)

(** Depending on the needs, the type [t] may be exposed or not. It can be
    anything required by the implementation. *)
type t

(** Somehow there must be a way exposed to construct a provider, decorated with
    the tags that the provider implements. Here, the construct
    [[Directory_reader.tag | File_reader.tag ]] means that this provider
    implements both interfaces: {!module:Directory_reader} and
    {!module:File_reader}. *)
val make
  :  env:< fs : _ Eio.Path.t ; .. >
  -> [ Directory_reader.tag | File_reader.tag ] Provider.t

(** In this specific example, we chose to expose the signature of the provider's
    implementation. This is not something that is usually required, since
    {!make} already provides a way to build a provider. This is only done here
    for the sake of the tests, as we show different ways an interface can be
    built based on various parts of its implementation. *)
module Impl : sig
  type nonrec t = t

  include Directory_reader.Provider_interface.S with type t := t
  include File_reader.Provider_interface.S with type t := t
end

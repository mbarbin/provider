(** [Eio_reader] is a provider for the {!module:Test_interfaces.Directory_reader}
    and {!module:Test_interfaces.File_reader} interfaces, based on [Eio].

    It is a thin wrapper around [Eio.Path], meant to demonstrate how to
    implement a provider for a given interface.

    {!module:Test_providers.Unix_reader} is another provider for the
    {!module:Test_interfaces.Directory_reader} interface. *)

(** Depending on the needs, the type [t] may be exposed or not. It can be
    anything required by the implementation. *)
type t

(** Somehow there must be a way exposed to construct a provider, decorated with
    the tags that the provider implements. Here, the construct
    [[ `Directory_reader | `File_reader ]] means that this provider
    implements both interfaces: {!module:Test_interfaces.Directory_reader} and
    {!module:Test_interfaces.File_reader}. *)
val make
  :  env:< fs : _ Eio.Path.t ; .. >
  -> [ Test_interfaces.Directory_reader.tag | Test_interfaces.File_reader.tag ]
       Provider.packed

(** In this specific example, we chose to expose the signature of the provider's
    implementation. This is not something that is usually required, since
    {!make} already provides a way to build a provider. This is only done here
    for the sake of the tests, as we show different ways an interface can be
    built based on various parts of its implementation. *)
module Impl : sig
  type nonrec t = t

  include Test_interfaces.Directory_reader.Provider_interface.S with type t := t
  include Test_interfaces.File_reader.Provider_interface.S with type t := t
end

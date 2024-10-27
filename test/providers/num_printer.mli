(** [Num_printer] is a provider for the {!module:Interface.Int_printer} and
    {!module:Interface.Float_printer} interfaces.

    The structure of this file is very similar to the [Eio_reader] module, thus
    is not documented in details. Refer to {!Eio_reader} for more. *)

(** In this case we decided to expose the type {!type:t} and {!val:handler}, to
    demonstrate how to override a particular binding. See [test__override.ml]. *)
type t = unit

val handler
  : (t, [ Interface.Int_printer.tag | Interface.Float_printer.tag ]) Provider.Handler.t

(** If you simply wish to use this provider without overrides, use [make ()]. *)
val make : t -> [ Interface.Int_printer.tag | Interface.Float_printer.tag ] Provider.t

(** In this specific example, we chose to expose the signature of the provider's
    implementation. This is not something that is usually required, since
    {!make} already provides a way to build a provider. This is only done here
    for the sake of the tests, as we show different ways an interface can be
    built based on various parts of its implementation. *)
module Impl : sig
  type nonrec t = t

  include Interface.Int_printer.Provider_interface.S with type t := t
  include Interface.Float_printer.Provider_interface.S with type t := t
end

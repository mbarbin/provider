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

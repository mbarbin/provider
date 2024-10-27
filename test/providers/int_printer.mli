(** [Int_printer] is a provider for the {!module:Interface.Int_printer} interface.

    The structure of this file is very similar to the [Eio_reader] module, thus
    is not documented in details. Refer to {!Eio_reader} for more.

    We use it as an alternate to [Num_printer], which is able to print both ints and floats
    to demonstrate cases involving subtyping. *)

type t = unit

val make : t -> Interface.Int_printer.tag Provider.t

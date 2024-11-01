(** [Int_printer] is a provider for {!module:Test_interfaces.Int_printer}.

    The structure of this file is very similar to the [Eio_reader] module, thus
    is not documented in details. Refer to [Eio_reader] for more info.

    We use it as an alternate to [Num_printer], which is able to print both
    integers and floats to demonstrate cases involving sub typing. *)

type t = unit

val make : t -> Test_interfaces.Int_printer.tag Provider.packed

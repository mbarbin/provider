(** An interface that is able to print ints.

    The structure of this file is very similar to the [Directory_reader] module,
    thus is not documented in details. Refer to {!Directory_reader} for more. *)

type tag = [ `Int_printer ]
type 'a t = ([> tag ] as 'a) Provider.t

val print : _ t -> int -> unit

module Provider_interface : sig
  module type S = sig
    type t

    val string_of_int : t -> int -> string
  end

  val int_printer : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t
  val make : (module S with type t = 't) -> ('t, tag) Provider.Handler.t
end

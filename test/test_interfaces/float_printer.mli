(** An interface that is able to print floats.

    The structure of this file is very similar to the [Directory_reader] module,
    thus is not documented in details. Refer to {!Directory_reader} for more. *)

type tag = [ `Float_printer ]
type 'a t = ([> tag ] as 'a) Provider.t

val print : _ t -> float -> unit

module Provider_interface : sig
  module type S = sig
    type t

    val string_of_float : t -> float -> string
  end

  val make : (module S with type t = 't) -> ('t, tag) Provider.Handler.t
  val float_printer : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t
end

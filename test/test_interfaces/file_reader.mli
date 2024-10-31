(** An interface that is able to load the contents of a file from the file system.

    The structure of this file is very similar to the [Directory_reader] module,
    thus is not documented in details. Refer to {!Directory_reader} for more. *)

type tag = [ `File_reader ]
type 'a t = ([> tag ] as 'a) Provider.t

val load : _ t -> path:string -> string

module Provider_interface : sig
  module type S = sig
    type t

    val load : t -> path:string -> string
  end

  val file_reader : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t
end

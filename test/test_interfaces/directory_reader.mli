(** A directory reader is a Trait that is able to list all the entries contained
    in a directory in the file system. *)

(** The tag is used as a phantom type to characterize the nature of the specific
    provider that this library needs. *)
type tag = [ `Directory_reader ]

(** This library can operate on any provider, as long as it implements at
    least the [`Directory_reader] capability. *)
type 'a t = ([> tag ] as 'a) Provider.t

(** This function will result in calling the method [readdir] implemented by the
    provider. *)
val readdir : _ t -> path:string -> string list

(** This function will result in calling the method [readdir] implemented by the
    provider, and then filtering the results to only keep the files with the
    given extension. This shows an example of a function that is implemented
    on top of a minimal interface furnished by a provider. Its implementation
    is shared across providers, since it lives in the implementation of this
    module. *)
val find_files_with_extension : _ t -> path:string -> ext:string -> string list

(** {1 Building providers} *)

(** Such abstraction will export a way for implementers (providers) to provide
    the implementation required by that interface. In [Eio] naming
    conventions, these modules are typically named [Pi]. *)
module Provider_interface : sig
  module type S = sig
    (** The implementation may be based on any type it wants, that's the whole point. *)
    type t

    (** Note that not all the interface exported by the module is required for
        the implementer, indeed only the minimal set of methods upon which we
        can build all the others. In this case, all that is required is the
        ability to list entries from a given directory. *)
    val readdir : t -> path:string -> string list
  end

  (** [make (module Impl)] creates a provider handler that implements the
      directory_reader functionality. Another option is to use the constructor
      [Directory_reader] below. *)
  val make : (module S with type t = 't) -> ('t, tag) Provider.Handler.t

  (** The actual trait constructor may or may not be exported by the provider
      interface -- either way works. That's left as a programmer's preference
      depending on the context. When this constructor is exported, you can use
      it in conjunction with {!Provider.Trait.implement}. *)
  val directory_reader : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t
end

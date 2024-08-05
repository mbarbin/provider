(** Provider - Dynamic Dispatch with Traits.

    A "provider" is a construct that implements a set of functionality that an
    library typically needs in order to provide certain functionality to a
    end user.

    The module is divided into several submodules:
    - {!module:Trait}: To identify functionality.
    - {!module:Binding}: Associates a Trait with an implementation for it.
    - {!module:Handler}: Manages the set of Traits that a provider implements.
    - {!module:Private}: Used for testing purposes.

    This module is inspired by the [Eio.Resource] module and provides a way to
    parameterize code when a library either doesn't want to or can't commit to a
    specific implementation. *)

module Trait : sig
  (** Think of a Trait as a way to designate the signature of a module that
      contains enough functions to support some functionality. The type
      {!type:t} allows to identify a Trait within the provider system. The
      name was inspired from the Rust programming language construct of the
      same name.

      - ['t] is the internal state of the provider itself.
      - ['module_type] is the signature of a module implementing the Trait.
      - ['tag] is the tag (or tags) indicating the supported Trait. It's a
        phantom type designed to make {!val:Handler.lookup} more type-safe.

      ['module_type] is expected to be a module type (Eio supports single
      functions but this is discouraged through the use of this library). *)
  type ('t, 'module_type, 'tag) t = ('t, 'module_type, 'tag) Trait0.t = ..

  (** {1 Dump & debug} *)

  module Info : sig
    (** This type is primarily used for debugging purposes.

        An [t] value includes the name of the Trait constructor and the module
        path where it was defined. It may also include the runtime id for the
        extensible variant of the Trait, but this is not included by default as
        its value can be brittle (it may depend on the order in which modules
        are evaluated).

        This type provides a way to retrieve and display detailed information
        about a Trait, which can be useful for debugging and understanding the
        structure and behavior of the provider system. *)
    type t

    val sexp_of_t : t -> Sexp.t

    (** Controls whether the runtime ids are shown or hidden in the sexp built
        by {!val:sexp_of_t}. By default [Fn.const (Sexp.Atom "#id")]. You may
        temporarily change it, e.g. in a test, for example using
        [Ref.set_temporarily]. *)
    val sexp_of_id : (int -> Sexp.t) ref
  end

  val info : _ t -> Info.t

  (** {1 Indexation} *)

  module Uid : sig
    type t = private int

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end

  (** A uid is particularly useful when you need to quickly look up or sort
      Traits, as it provides a consistent and unique way to identify each
      Trait. You can use it to manipulate Traits within container structures,
      making it easier to store, retrieve, and compare Traits at runtime.

      Trait uniq ids are computed with [Obj.Extension_constructor.id], applied
      to the constructors of the variant type {!type:Trait.t}, making them valid
      only for the lifetime of the running program. *)
  val uid : _ t -> Uid.t

  val same : _ t -> _ t -> bool

  (** [implement trait ~impl:(module Impl)] says to implement [trait] with
      [Impl]. The module [Impl] provided must have the right module type as
      specified by the type of [trait].

      The tags associated with the [trait] are ignored at this stage. The
      handling of the tags happens at the handler building stage, not at the
      granularity of each Trait. This means that the {!val:implement} function
      focuses solely on creating the implementation, without considering the
      tags that indicate which Traits are supported by the provider. *)
  val implement : ('t, 'module_type, _) t -> impl:'module_type -> 't Binding0.t
end

module Binding : sig
  (** A binding associates a Trait with an implementation for it. *)
  type 'a t = 'a Binding0.t = private
    | T :
        { trait : ('t, 'module_type, _) Trait.t
        ; implementation : 'module_type
        }
        -> 't t

  (** {1 Dump & debug} *)

  val uid : _ t -> Trait.Uid.t
  val info : _ t -> Trait.Info.t
end

module Handler : sig
  (** Manipulating the set of Traits implemented by a provider. *)

  (** A handler is essentially a collection of bindings, associating each Trait
      it contains with an implementation for it. Each Trait provides a
      specific functionality (one Trait implementation = one first-class
      module with type t = 't).

      - ['t] is the internal state of the provider.
      - ['tags] indicate which functionality are supported by the provider. It
        is a phantom type using polymorphic variants. To give an example, in the
        tests for this library, we have two modules defining each their own tag:

      {[
        module Directory_reader = struct
          type tag = [ `Directory_reader ]
        end

        module File_reader = struct
          type tag = [ `File_reader ]
        end
      ]}

      Then, the type of the handler for a provider whose internal state is
      [state], that would implement both functionalities would be:

      {[
        (state, [ Directory_reader.tag | File_reader.tag ]) Provider.Handler.t
      ]} *)
  type ('t, -'tags) t

  (** {1 Building handlers} *)

  (** [make bindings] create a new handler from a list of bindings. It only
      keeps the last implementation supplied for each Trait, from left to
      right. This means that the resulting handler will not contain any
      duplicate Traits, and the order of the bindings in the input list can
      affect its contents. *)
  val make : 't Binding.t list -> ('t, _) t

  (** [bindings t] returns a list of bindings with the Traits that the handler
      [t] supports. See also {!extend}. *)
  val bindings : ('t, _) t -> 't Binding.t list

  (** [extend t ~with_] extends the handler [t] and returns a new handler that
      includes both the original and additional bindings. The resulting
      handler only contains the last occurrence of each Trait, prioritizing
      the rightmost elements in the combined list [bindings t @ with_]. *)
  val extend : ('t, _) t -> with_:'t Binding.t list -> ('t, _) t

  (** {1 Lookup}

      A lookup operation is used to retrieve the implementation of a specific
      Trait within an handler. *)

  (** [is_empty t] checks if an handler [t] implements any Traits. An empty
      handler may be created using [make []]. It will cause any lookup
      operation to fail. It can be useful for initializing data structures or
      providing a base case for algorithms that process handlers. *)
  val is_empty : ('t, _) t -> bool

  (** [lookup t ~trait] retrieves the implementation for a given [trait] from an
      handler.

      If the provider has correctly exported their implementation using the
      appropriate tags, the compiler will ensure that this function does not
      fail in user code (a failure of this function would typically indicate a
      programming error in the provider's setup). *)
  val lookup
    :  ('t, 'tags) t
    -> trait:('t, 'implementation, 'tags) Trait.t
    -> 'implementation

  (** [lookup_opt t ~trait] returns the implementation of the [trait]
      ([Some implementation]) or indicates that the Trait is not implemented
      ([None]).

      This is particularly useful in scenarios where a part of a program needs
      to adapt behavior at runtime based on whether certain functionalities are
      available or not. *)
  val lookup_opt
    :  ('t, _) t
    -> trait:('t, 'implementation, _) Trait.t
    -> 'implementation option

  (** [implements t ~trait] says wether an handler implements a Trait. This
      is [true] iif [lookup_opt t ~trait] returns [Some _]. *)
  val implements : ('t, _) t -> trait:('t, _, _) Trait.t -> bool
end

(** A provider is a pair of a value and a set of Traits that the provider
    implements. *)
type -'tags t =
  | T :
      { t : 't
      ; handler : ('t, 'tags) Handler.t
      }
      -> 'tags t

module Private : sig
  (** This module is exported for testing purposes only.

      Its interface may change in breaking ways without requiring a major
      version of the library to be minted. Do not use. *)

  module Handler : sig
    (** [same_trait_uids i1 i2] checks if the Traits of two handlers are the
        same and in the same order. *)
    val same_trait_uids : ('t1, _) Handler.t -> ('t2, _) Handler.t -> bool

    (** Exported to test the caching strategy. Retains the most recently looked
        up Trait. Currently returns [None] for empty handler, and if the
        handler is not empty, returns the most recently looked up Trait
        ([Some uid]) or an arbitrary initial value. *)
    val cache : _ Handler.t -> Trait.Uid.t option

    (** Part of the strategy for [make], [extend], etc. *)
    val dedup_sorted_keep_last : 'a list -> compare:('a -> 'a -> int) -> 'a list
  end

  module Import : sig
    (** Exported things from the import module we'd like to test separately. *)

    module Array : sig
      val for_alli : 'a array -> f:(int -> 'a -> bool) -> bool
    end

    module Ordering : sig
      type t =
        | Less
        | Equal
        | Greater

      val of_int : int -> t
    end
  end
end

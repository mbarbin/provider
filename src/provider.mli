(** Provider - Dynamic Dispatch with Traits.

    A "provider" is a construct that implements a set of functionality that an
    library typically needs in order to provide certain functionality to a
    client.

    The module is divided into several submodules:
    - {!module:Trait}: To identify and implement functionality.
    - {!module:Implementation}: Represents an implementation for a trait.
    - {!module:Interface}: Manages the set of traits that a provider implements.
    - {!module:Private}: Used for testing purposes.

    This module is inspired by the [Eio.Resource] module and provides a way to
    parameterize code when a library either doesn't want to or can't commit to a
    specific implementation. *)

module Trait : sig
  (** Think of a trait as a way to identify and implement the signature of a
      module that contains enough functions to support some functionality. The
      type {!type:t} allows to identify a trait within the provider system.
      The name was inspired from the Rust programming language construct of
      the same name.

      - ['t] is the internal state of the provider itself.
      - ['module_type] is the signature of a module implementing the trait.
      - ['tag] is the tag (or tags) indicating the supported trait. It's a
        phantom type designed to make {!val:Interface.lookup} more type-safe.
        This relates to Trait bounds in Rust.

      ['module_type] is expected to be a module type (Eio supports single
      functions but this is discouraged through the use of this library). *)
  type ('t, 'module_type, 'tag) t = ('t, 'module_type, 'tag) Trait0.t = ..

  (** {1 Dump & debug} *)

  module Info : sig
    (** This type is primarily used for debugging purposes.

        An [t] value includes the name of the trait constructor and the module
        path where it was defined. It may also include the runtime id for the
        extensible variant of the trait, but this is not included by default as
        its value can be brittle (it may depend on the order in which modules
        are evaluated).

        This type provides a way to retrieve and display detailed information
        about a trait, which can be useful for debugging and understanding the
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
      traits, as it provides a consistent and unique way to identify each
      trait. You can use it to manipulate traits within container structures,
      making it easier to store, retrieve, and compare traits at runtime.

      Trait uniq ids are computed with [Obj.Extension_constructor.id], applied
      to the constructors of the variant type {!type:Trait.t}, making them valid
      only for the lifetime of the running program. *)
  val uid : _ t -> Uid.t

  val same : _ t -> _ t -> bool

  (** [implement trait ~impl:(module Impl)] says to implement [trait] with
      [Impl]. The module [Impl] provided must have the right module type as
      specified by the type of [trait].

      The tags associated with the [trait] are ignored at this stage. The
      handling of the tags happens at the interface building stage, not at the
      granularity of each trait. This means that the {!val:implement} function
      focuses solely on creating the implementation, without considering the
      tags that indicate which traits are supported by the provider. *)
  val implement : ('t, 'module_type, _) t -> impl:'module_type -> 't Implementation0.t
end

module Implementation : sig
  (** Representing an implementation for a trait. *)

  type 'a t = 'a Implementation0.t = private
    | T :
        { trait : ('t, 'module_type, _) Trait.t
        ; impl : 'module_type
        }
        -> 't t

  (** {1 Dump & debug} *)

  val uid : _ t -> Trait.Uid.t
  val info : _ t -> Trait.Info.t
end

module Interface : sig
  (** Manipulating the set of traits implemented by a provider.

      This module provides functions for creating an interface, as well as
      retrieving and extending the traits implemented by an interface, making it
      easy to manage the functionalities that a provider supports. *)

  (** An interface is essentially a collection of traits that a provider
      implements, each of which providing a specific functionality (one trait
      implementation = one first-class module with type t = 't).

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

      Then, the type of the interface for a provider whose internal state is
      [state], that would implement both functionalities would be:

      {[
        (state, [ Directory_reader.tag | File_reader.tag ]) Provider.Interface.t
      ]} *)
  type ('t, -'tags) t

  (** {1 Building interfaces} *)

  (** [make implementations] create a new interface from a list of
      implementation. It only keeps the last implementation supplied for each
      trait. This means that the resulting interface will not contain any
      duplicate traits, and the order of the implementations in the input list
      can affect its contents. *)
  val make : 't Implementation.t list -> ('t, _) t

  (** [implementations t] returns a list of trait implementations that the
      interface [t] supports. See also {!extend}. *)
  val implementations : ('t, _) t -> 't Implementation.t list

  (** [extend t ~with_] extends the interface [t] and returns a new interface
      that includes both the original and additional implementations. The
      resulting interface only contains the last occurrence of each trait,
      prioritizing the rightmost elements in the combined list
      [implementations t @ with_]. *)
  val extend : ('t, _) t -> with_:'t Implementation.t list -> ('t, _) t

  (** {1 Lookup}

      A lookup operation is used to retrieve the implementation of a specific
      trait within an interface. *)

  (** [is_empty t] checks if an interface [t] implements any traits. An empty
      interface may be created using [make []]. It will cause any lookup
      operation to fail. It can be useful for initializing data structures or
      providing a base case for algorithms that process interfaces. *)
  val is_empty : ('t, _) t -> bool

  (** [lookup t ~trait] retrieves the implementation for a given [trait] from an
      interface.

      If the provider has correctly exported their implementation using the
      appropriate tags, the compiler will ensure that this function does not
      fail in user code (a failure of this function would typically indicate a
      programming error in the provider's setup). *)
  val lookup
    :  ('t, 'tags) t
    -> trait:('t, 'implementation, 'tags) Trait.t
    -> 'implementation

  (** [lookup_opt t ~trait] returns the implementation of the [trait]
      ([Some implementation]) or indicates that the trait is not implemented
      ([None]).

      This is particularly useful in scenarios where a part of a program needs
      to adapt behavior at runtime based on whether certain functionalities are
      available or not. *)
  val lookup_opt
    :  ('t, _) t
    -> trait:('t, 'implementation, _) Trait.t
    -> 'implementation option

  (** [implements t ~trait] says wether an interface implements a trait. This
      is [true] iif [lookup_opt t ~trait] returns [Some _]. *)
  val implements : ('t, _) t -> trait:('t, _, _) Trait.t -> bool
end

(** A provider is a pair of a value and a set of traits that the provider
    implements. *)
type -'tags t =
  | T :
      { t : 't
      ; interface : ('t, 'tags) Interface.t
      }
      -> 'tags t

module Private : sig
  (** This module is exported for testing purposes only.

      Its interface may change in breaking ways without requiring a major
      version of the library to be minted. Do not use. *)

  module Interface : sig
    (** [same_trait_uids i1 i2] checks if the traits of two interfaces are the
        same and in the same order. *)
    val same_trait_uids : ('t1, _) Interface.t -> ('t2, _) Interface.t -> bool

    (** Exported to test the caching strategy. Retains the most recently looked
        up trait. Currently returns [None] for empty interface, and if the
        interface is not empty, returns the most recently looked up trait
        ([Some uid]) or an arbitrary initial value. *)
    val cache : _ Interface.t -> Trait.Uid.t option

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
        | Equal
        | Less
        | Greater

      val of_int : int -> t
    end
  end
end

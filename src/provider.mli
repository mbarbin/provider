(** Provider - Dynamic Dispatch with Traits.

    A provider is a construct that implements a set of functionality that a
    library typically needs in order to provide certain functionality to a
    end user.

    The module is divided into several submodules:
    - {!module:Trait}: To identify functionality.
    - {!module:Binding}: Associates a Trait with an implementation for it.
    - {!module:Private}: Used for testing purposes.

    The main module provides the following functionalities:
    - Implement and lookup traits.
    - Manages the set of Traits that a provider implements.

    This module is inspired by the [Eio.Resource] module and provides a way to
    parametrize code when a library either doesn't want to or can't commit to a
    specific implementation. *)

module Trait : sig
  (** Think of a Trait as a way to designate the signature of a module that
      contains enough functions to support some functionality. The type
      {!type:t} allows to identify a Trait within the provider system. The
      name was inspired from the Rust programming language construct of the
      same name.

      - ['t] is the internal type on which the provider traits operate.
      - ['module_type] is the signature of a module implementing the Trait.
      - ['tag] is the tag (or tags) indicating the supported Trait(s). It's a
        phantom type designed to help using {!val:lookup} correctly.

      ['module_type] is typically expected to be a module type, but it doesn't
      have too (functions, constants are fine too, etc.). *)
  type ('t, 'module_type, 'tag) t = ('t, 'module_type, 'tag) Trait0.t

  (** {1 Creating traits}

      Traits are abstract and must be created using the following functors. The
      most common one is {!module:Create}. It is to be used when the trait is
      defined by a module type with a single type t. For example:

      {[
        module type Show = sig
          type t

          val show : t -> string
        end

        module Show : sig
          val t : ('a, (module Show with type t = 'a), [> `Show ]) Provider.Trait.t
        end = Provider.Trait.Create (struct
            type 'a module_type = (module Show with type t = 'a)
          end)
      ]}

      The other functors are reserved for less common cases. The number suffix
      indicates the number of parameters of the [module_type] type, each of
      which must be present and injective in [X.t]. We added one extra parameter
      to [X.t] to allow for more flexibility in what can be expressed, but not
      all parameters have to be used. *)

  module Create (X : sig
      type 'a module_type
    end) : sig
    val t : ('a, 'a X.module_type, _) t
  end

  module Create0 (X : sig
      type 'a t
      type module_type
    end) : sig
    val t : ('a X.t, X.module_type, _) t
  end

  module Create1 (X : sig
      type (!'a, 'b) t
      type 'a module_type
    end) : sig
    val t : (('a, 'b) X.t, 'a X.module_type, _) t
  end

  module Create2 (X : sig
      type (!'a, !'b, 'c) t
      type ('a, 'b) module_type
    end) : sig
    val t : (('a, 'b, 'c) X.t, ('a, 'b) X.module_type, _) t
  end

  (** {1 Dump & debug} *)

  module Info : sig
    (** Displaying debugging information about a trait.

        This module provides a way to register, retrieve and display detailed
        information about a Trait, which can be useful for debugging and
        understanding the structure and behavior of the provider system.

        This is meant for debugging purposes only. *)

    (** A [t] value includes a unique runtime id for the trait, as well as an
        optional name that may be registered by the user. The id is not shown
        by [sexp_of_t] by default because its value can be brittle (it may
        depend on the order in which modules are evaluated). To display ids,
        see {!val:sexp_of_id}. *)
    type t

    val sexp_of_t : t -> Sexp.t

    (** Controls whether the runtime ids are shown or hidden in the sexp built
        by {!val:sexp_of_t}. By default [Fn.const (Sexp.Atom "#id")]. You may
        temporarily change it, e.g. in a test, for example using
        [Ref.set_temporarily]. *)
    val sexp_of_id : (int -> Sexp.t) ref

    (** Register a string mnemonic to attach to the trait for display purposes.
        By default, trait do not have any name. *)
    val register_name : _ Trait0.t -> name:string -> unit
  end

  val info : _ t -> Info.t

  (** {1 Indexation} *)

  module Uid : sig
    type t = private int

    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val seeded_hash : int -> t -> int
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

(** {1 Implementing Traits} *)

(** [implement trait ~impl:(module Impl)] says to implement [trait] with [Impl].
    The module [Impl] provided must have the right module type as specified by
    the type of [trait].

    The tags associated with the [trait] are ignored at this stage. The handling
    of the tags happens at the provider building stage, not at the granularity
    of each Trait. This means that the {!val:implement} function focuses solely
    on creating the implementation, without considering the tags that indicate
    which Traits are supported by the provider. *)
val implement : ('t, 'module_type, _) Trait.t -> impl:'module_type -> 't Binding.t

(** {1 Building providers} *)

(** A provider is essentially a collection of bindings, associating each Trait
    it contains with an implementation for it. Each Trait provides a
    specific functionality (one Trait implementation = one first-class
    module with type t = 't).

    - ['t] is the internal state of the provider.
    - ['tags] indicate which functionality are supported by the provider. It is
      a phantom type using polymorphic variants. To give an example, in the
      tests for this library, we have two modules defining each their own tag:

    {[
      module Directory_reader = struct
        type tag = [ `Directory_reader ]
      end

      module File_reader = struct
        type tag = [ `File_reader ]
      end
    ]}

    Then, the type of a provider whose internal type is [state], implementing
    both Traits would be:

    {[
      (state, [ Directory_reader.tag | File_reader.tag ]) Provider.t
    ]} *)
type ('t, -'tags) t

(** [make bindings] create a new provider from a list of bindings. It only keeps
    the last implementation supplied for each Trait, from left to right. This
    means that the resulting provider will not contain any duplicate Traits,
    and the order of the bindings in the input list can affect its contents. *)
val make : 't Binding.t list -> ('t, _) t

(** [bindings t] returns a list of bindings with the Traits that the provider
    [t] supports. See also {!extend}. *)
val bindings : ('t, _) t -> 't Binding.t list

(** [extend t ~with_] extends the provider [t] and returns a new provider that
    includes both the original and additional bindings. The resulting provider
    only contains the last occurrence of each Trait, prioritizing the
    rightmost elements in the combined list [bindings t @ with_]. *)
val extend : ('t, _) t -> with_:'t Binding.t list -> ('t, _) t

(** {1 Lookup}

    A lookup operation is used to retrieve the implementation of a specific
    Trait implementation from a provider. *)

(** [is_empty t] checks if a provider [t] implements any Traits. An empty
    provider may be created using [make []]. It will cause any lookup
    operation to fail. It can be useful for initializing data structures or
    providing a base case for algorithms that process providers. *)
val is_empty : ('t, _) t -> bool

(** [lookup t ~trait] retrieves the implementation for a given [trait] from a
    provider.

    If the provider has correctly exported their implementation using the
    appropriate tags, the compiler will ensure that this function does not fail
    in user code (a failure of this function would typically indicate a
    programming error in the provider's setup).

    In the rare case where a provider has not correctly exported the tags of
    their implementation, this function will raise an internal exception. The
    exception is not exported, because it is not raised assuming a correct usage
    of the library.

    You can find examples of incorrect usage in the tests of this library (e.g.
    "test__invalid_tags.ml"). *)
val lookup
  :  ('t, 'tags) t
  -> trait:('t, 'implementation, 'tags) Trait.t
  -> 'implementation

(** [lookup_opt t ~trait] returns the implementation of the [trait] if present
    ([Some implementation]) or indicates that the Trait is not implemented
    ([None]).

    This is particularly useful in scenarios where a part of a program needs to
    adapt behavior at runtime based on whether certain functionalities are
    available or not. *)
val lookup_opt
  :  ('t, _) t
  -> trait:('t, 'implementation, _) Trait.t
  -> 'implementation option

(** [implements t ~trait] says wether a provider implements a Trait. This is
    [true] iif [lookup_opt t ~trait] returns [Some _]. *)
val implements : ('t, _) t -> trait:('t, _, _) Trait.t -> bool

(** A packed provider is a pair of a value and a set of Traits that a provider
    implements on that value. This is an OCaml value that behaves roughly like
    an object, but isn't one. *)
type -'tags packed =
  | T :
      { t : 't
      ; provider : ('t, 'tags) t
      }
      -> 'tags packed

module Private : sig
  (** This module is exported for testing purposes only.

      Its interface may change in breaking ways without requiring a major
      version of the library to be minted. Do not use. *)

  (** [same_trait_uids i1 i2] checks if the Traits of two providers are the same
      and in the same order. *)
  val same_trait_uids : ('t1, _) t -> ('t2, _) t -> bool

  (** Exported to test the caching strategy. Retains the most recently looked up
      Trait. Currently returns [None] for empty provider, and if the provider
      is not empty, returns the most recently looked up Trait ([Some uid]) or
      an arbitrary initial value. *)
  val cache : _ t -> Trait.Uid.t option

  (** Part of the strategy for [make], [extend], etc. *)
  val dedup_sorted_keep_last : 'a list -> compare:('a -> 'a -> int) -> 'a list

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

  module Trait0 = Trait0
end

(** Manipulating values that behave like objects, without using OCaml's built-in
    object system.

    A "provider" is a construct that implements a set of methods that an
    interface typically needs in order to provide certain functionality to a
    client.

    The module is divided into several submodules:
    - {!module:Class_id}: To identify classes.
    - {!module:Class}: For implementing classes.
    - {!module:Interface}: Manages the set of classes that an object implements.
    - {!module:Private}: Used for testing purposes.

    This module is inspired by the [Eio.Resource] module and provides a way to
    parameterize code when a library either doesn't want to or can't commit to a
    specific implementation. *)

module Class_id : sig
  (** Identifying a class within the provider system.

      - ['t] is the internal state of the provider itself.
      - ['implementation] is the API that can be requested.
      - ['tag] is the tag (or tags) indicating the supported class. It's a
        phantom type designed to make {!val:Interface.lookup} more type-safe.

      The API requested is expected to be a module type (Eio supports single
      functions but this is discouraged through the use of this library). *)
  type ('t, 'implementation, 'tag) t = ..

  (** {1 Dump & debug} *)

  module Info : sig
    (** This type is primarily used for debugging purposes.

        An [t] value includes the name of the class constructor and the module
        path where it was defined. It may also include the runtime id for the
        extensible variant of the class id, but this is not included by default
        as its value can be brittle (it may depend on the order in which modules
        are evaluated).

        This type provides a way to retrieve and display detailed information
        about a class, which can be useful for debugging and understanding the
        structure and behavior of the provider system. *)
    type t [@@deriving sexp_of]

    (** Controls whether the runtime ids are shown or hidden in the sexp built
        by {!val:sexp_of_t}. By default [Fn.const (Sexp.Atom "#id")]. You may
        temporarily change it, e.g. in a test, for example using
        {!Ref.set_temporarily}. *)
    val sexp_of_id : (int -> Sexp.t) ref
  end

  val info : _ t -> Info.t

  (** {1 Indexation} *)

  module Uid : sig
    (** A uid is particularly useful when you need to quickly look up or sort
        classes, as it provides a consistent and unique way to identify each
        class. You can use it to manipulate classes within container
        structures, making it easier to store, retrieve, and compare classes
        at runtime. *)
    type t [@@deriving compare, equal, hash, sexp_of]

    include Comparable.S with type t := t
  end

  val uid : _ t -> Uid.t
  val same : _ t -> _ t -> bool
end

module Class : sig
  (** This module is used by providers to implement classes. *)

  type _ t = private
    | T :
        { class_id : ('t, 'implementation, _) Class_id.t
        ; implementation : 'implementation
        }
        -> 't t

  (** [implement ~class_id (module Impl)] returns a class that uses [Impl] as
      the implementation for [class_id].

      It's important to note that at this stage, the tags associated with the
      [class_id] are ignored. The handling of the tags happens at the interface
      level, not at the granularity of each class. This means that the
      [implement] function focuses solely on creating the class, without
      considering the tags that indicate which classes are supported by the
      provider. *)
  val implement : class_id:('t, 'implementation, _) Class_id.t -> 'implementation -> 't t

  (** {1 Dump & debug} *)

  val uid : _ t -> Class_id.Uid.t
  val info : _ t -> Class_id.Info.t
end

module Interface : sig
  (** Manipulating the set of classes implemented by a provider.

      This module provides functions for creating an interface, as well as
      retrieving and extending the classes implemented by an interface, making
      it easy to manage the functionalities that a provider supports. *)

  (** An interface is essentially a collection of classes that an object
      implements, each of which providing a specific set of functionalities
      (one class = one first-class module with type t = 't).

      - ['t] is the internal state of the provider.
      - ['tags] indicate which functionality are supported by the interface. It
        is a phantom type using polymorphic variants. To give an example, in the
        tests for this library, we have two interfaces defining each their own
        tag:

      {[
        module Directory_reader = struct
          type tag = [ `Directory_reader ]
        end

        module File_reader = struct
          type tag = [ `File_reader ]
        end
      ]}

      Then, the type of the interface for a provider whose internal state is
      [state], that would implement both interfaces would be:

      {[
        (state, [ Directory_reader.tag | File_reader.tag ]) Provider.Interface.t
      ]} *)
  type ('t, -'tags) t

  (** {1 Building interfaces} *)

  (** [make classes] create a new interface from a list of classes. It only
      keeps the last occurrence of each class. This means that the resulting
      interface will not contain any duplicate classes, and the order of the
      classes in the input list can affect the contents of the resulting
      interface. *)
  val make : 't Class.t list -> ('t, _) t

  (** [classes t] returns a list of classes that the interface [t]
      implements. See also {!extend}. *)
  val classes : ('t, _) t -> 't Class.t list

  (** [extend t ~with_] extends the interface [t] and returns a new interface
      that includes both the original and additional classes. The resulting
      interface only contains the last occurrence of each class identifier,
      prioritizing the rightmost elements in the combined list
      [classes t @ with_]. *)
  val extend : ('t, _) t -> with_:'t Class.t list -> ('t, _) t

  (** {1 Lookup}

      A lookup operation is used to retrieve the implementation of a specific
      class within an interface based on its class identifier. *)

  (** [is_empty t] checks if an interface [t] implements any classes. An empty
      interface may be created using [make []]. It will cause any lookup
      operation to fail. It can be useful for initializing data structures or
      providing a base case for algorithms that process interfaces. *)
  val is_empty : ('t, _) t -> bool

  (** [lookup t ~class_id] retrieves the implementation for a given class from
      an interface.

      If the provider has correctly exported their implementation using the
      appropriate tags, the compiler will ensure that this function does not
      fail in user code (a failure of this function would typically indicate a
      programming error in the provider's setup). *)
  val lookup
    :  ('t, 'tags) t
    -> class_id:('t, 'implementation, 'tags) Class_id.t
    -> 'implementation

  (** [lookup_opt t ~class_id] returns the implementation of the class
      ([Some implementation]) or indicates that the class is not implemented
      ([None]).

      This is particularly useful in scenarios where a part of a program needs
      to adapt behavior at runtime based on whether certain functionalities are
      available or not. *)
  val lookup_opt
    :  ('t, _) t
    -> class_id:('t, 'implementation, _) Class_id.t
    -> 'implementation option

  (** [implements t ~class_id] says wether an interface implements a class. This
      is [true] iif [lookup_opt t ~class_id] returns [Some _]. *)
  val implements : ('t, _) t -> class_id:('t, _, _) Class_id.t -> bool
end

(** A provider is a pair of a value and an interface for it. Think about [t] as
    the internal state of an object, and [interface] as the set of methods
    that the object implements. *)
type -'tags t =
  | T :
      { t : 't
      ; interface : ('t, 'tags) Interface.t
      }
      -> 'tags t

module Private : sig
  (** This module is exported for testing purposes only.contents

      Its interface may change in breaking ways without requiring a major
      version of the library to be minted. Use at your own risk. *)

  module Interface : sig
    (** [same_class_uids i1 i2] checks if the class identifiers of two
        interfaces are the same and in the same order. *)
    val same_class_uids : ('t, _) Interface.t -> ('t, _) Interface.t -> bool

    (** Exported to test the caching strategy. Retains the most recently looked
        up class. Currently returns [None] for empty interface, and if the
        interface is not empty, returns the most recently looked up class
        ([Some uid]) or an arbitrary initial value. *)
    val cache : _ Interface.t -> Class_id.Uid.t option
  end
end

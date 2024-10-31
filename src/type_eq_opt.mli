(** A simplified type for [_ Type.eq option].

    Using the option variation can sometimes result in the option being
    optimized by the compiler (such as being pre-allocated, or code being
    inlined), which is desirable. However, we prefer this type because it
    eliminates the need to consider the conditions under which such
    optimizations are guaranteed.

    This module is intended for internal use within the library and is not
    designed to be a general-purpose module. *)

type (_, _) t =
  | Equal : ('a, 'a) t
  | Not_equal : ('a, 'b) t

(** A minimal higher-kinded library for the purpose of tests and tutorials.

    This is a reduced variation on https://github.com/janestreet/higher_kinded
    that we use to explore ways the Provider library may be used with interfaces
    containing parametrized types.

    We use this small kernel rather than an upstream library because we needed
    to add some injectivity annotations to the types, and the higher-kinded
    libraries available do not have them at this time. Proposing this change
    upstream would require more thoughts, and so far we didn't have actual usage
    for this (outside of tests and tutorials), so we went with this small kernel
    instead. *)

type !'a t
type !'a hk := 'a t

module type S = sig
  type 'a t
  type higher_kinded

  val inject : 'a t -> ('a -> higher_kinded) hk
  val project : ('a -> higher_kinded) hk -> 'a t
end

module Make (X : sig
    type !'a t
  end) : S with type 'a t := 'a X.t

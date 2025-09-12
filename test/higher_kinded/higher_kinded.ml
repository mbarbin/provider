(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

type !'a t
type !'a hk = 'a t

module type S = sig
  type 'a t
  type higher_kinded

  val inject : 'a t -> ('a -> higher_kinded) hk
  val project : ('a -> higher_kinded) hk -> 'a t
end

module Make (X : sig
    type 'a t
  end) : S with type 'a t := 'a X.t = struct
  type higher_kinded

  external inject : 'a X.t -> ('a -> higher_kinded) hk = "%identity"
  external project : ('a -> higher_kinded) hk -> 'a X.t = "%identity"
end

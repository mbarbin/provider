(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

(* @mdexp

   # Hello World

   Below is a minimalist program that uses the Provider library (cheat sheet).

   ## Trait Definition

   @mdexp.code *)

module type S = sig
  type t

  val show : t -> string
end

type show = [ `Show ]

module Show : sig
  val t : ('t, (module S with type t = 't), [> show ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 'a module_type = (module S with type t = 'a)
  end)

(* @mdexp

   ## Parametrized Code

   @mdexp.code *)

let print (Provider.T { t; provider }) =
  let module M = (val Provider.lookup provider ~trait:Show.t) in
  print_endline (M.show t)
;;

(* @mdexp

   ## Trait Implementation

   @mdexp.code *)

let string_provider t =
  let provider =
    Provider.make
      [ Provider.implement
          Show.t
          ~impl:
            (module struct
              type t = string

              let show = String.uppercase_ascii
            end)
      ]
  in
  Provider.T { t; provider }
;;

(* @mdexp

   ## Invocation

   @mdexp.code *)

let%expect_test "Hello World" =
  print (string_provider "Hello World");
  [%expect {| HELLO WORLD |}];
  ()
;;

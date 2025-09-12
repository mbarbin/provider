(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

type tag = [ `Int_printer ]
type 'a t = ([> tag ] as 'a) Provider.packed

module Provider_interface = struct
  module type S = sig
    type t

    val string_of_int : t -> int -> string
  end

  module Trait = Provider.Trait.Create (struct
      type 'a module_type = (module S with type t = 'a)
    end)

  let int_printer =
    (Trait.t : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t)
  ;;

  let () = Provider.Trait.Info.register_name int_printer ~name:"Int_printer"

  let make (type t) (module M : S with type t = t) =
    Provider.make [ Provider.implement int_printer ~impl:(module M) ]
  ;;
end

let print (Provider.T { t; provider }) i =
  let module M = (val Provider.lookup provider ~trait:Provider_interface.int_printer) in
  Stdlib.print_endline (M.string_of_int t i)
;;

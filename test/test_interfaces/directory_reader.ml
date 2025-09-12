(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

type tag = [ `Directory_reader ]
type 'a t = ([> tag ] as 'a) Provider.packed

module Provider_interface = struct
  module type S = sig
    type t

    val readdir : t -> path:string -> string list
  end

  module Trait = Provider.Trait.Create (struct
      type 'a module_type = (module S with type t = 'a)
    end)

  let directory_reader =
    (Trait.t : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t)
  ;;

  let () = Provider.Trait.Info.register_name directory_reader ~name:"Directory_reader"

  let make (type t) (module M : S with type t = t) =
    Provider.make [ Provider.implement directory_reader ~impl:(module M) ]
  ;;
end

let readdir (Provider.T { t; provider }) ~path =
  let module M = (val Provider.lookup provider ~trait:Provider_interface.directory_reader)
  in
  M.readdir t ~path
;;

(* The implementation of that function is the same regardless of the provider
   used. *)
let find_files_with_extension t ~path ~ext =
  let files = readdir t ~path in
  List.filter files ~f:(fun file -> String.is_suffix file ~suffix:ext)
;;

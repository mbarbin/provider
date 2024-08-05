type tag = [ `Directory_reader ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val readdir : t -> path:string -> string list
  end

  type (_, _, _) Provider.Trait.t +=
    | Directory_reader : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t

  let make (type t) (module M : S with type t = t) =
    Provider.Handler.make [ Provider.Trait.implement Directory_reader ~impl:(module M) ]
  ;;
end

let readdir (Provider.T { t; handler }) ~path =
  let module M =
    (val Provider.Handler.lookup handler ~trait:Provider_interface.Directory_reader)
  in
  M.readdir t ~path
;;

(* The implementation of that function is the same regardless of the provider
   used. *)
let find_files_with_extension t ~path ~ext =
  let files = readdir t ~path in
  List.filter files ~f:(fun file -> String.is_suffix file ~suffix:ext)
;;

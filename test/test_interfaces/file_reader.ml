type tag = [ `File_reader ]
type 'a t = ([> tag ] as 'a) Provider.packed

module Provider_interface = struct
  module type S = sig
    type t

    val load : t -> path:string -> string
  end

  module Trait = Provider.Trait.Create (struct
      type 'a module_type = (module S with type t = 'a)
    end)

  let file_reader =
    (Trait.t : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t)
  ;;
end

let () =
  Provider.Trait.Info.register_name Provider_interface.file_reader ~name:"File_reader"
;;

let load (Provider.T { t; provider }) ~path =
  let module M = (val Provider.lookup provider ~trait:Provider_interface.file_reader) in
  M.load t ~path
;;

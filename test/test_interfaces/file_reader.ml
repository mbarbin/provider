type tag = [ `File_reader ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val load : t -> path:string -> string
  end

  type (_, _, _) Provider.Trait.t +=
    | File_reader : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t

  let () = Provider.Trait.Info.register_name File_reader ~name:"File_reader"
end

let load (Provider.T { t; handler }) ~path =
  let module M =
    (val Provider.Handler.lookup handler ~trait:Provider_interface.File_reader)
  in
  M.load t ~path
;;

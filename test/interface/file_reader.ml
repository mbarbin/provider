type tag = [ `File_reader ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val load : t -> path:string -> string
  end

  type (_, _, _) Provider.Trait.t +=
    | File_reader : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t
end

let load (Provider.T { t; interface }) ~path =
  let module M =
    (val Provider.Interface.lookup interface ~trait:Provider_interface.File_reader)
  in
  M.load t ~path
;;

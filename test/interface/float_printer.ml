type tag = [ `Float_printer ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val string_of_float : t -> float -> string
  end

  type (_, _, _) Provider.Class_id.t +=
    | Float_printer : ('t, (module S with type t = 't), [> tag ]) Provider.Class_id.t

  let make (type t) (module M : S with type t = t) =
    Provider.Interface.make
      [ Provider.Class.implement ~class_id:Float_printer (module M) ]
  ;;
end

let print (Provider.T { t; interface }) f =
  let module M =
    (val Provider.Interface.lookup interface ~class_id:Provider_interface.Float_printer)
  in
  print_endline (M.string_of_float t f)
;;

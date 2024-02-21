type tag = [ `Float_printer ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val string_of_float : t -> float -> string
  end

  type (_, _, _) Provider.Trait.t +=
    | Float_printer : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t

  let make (type t) (module M : S with type t = t) =
    Provider.Interface.make [ Provider.Trait.implement Float_printer ~impl:(module M) ]
  ;;
end

let print (Provider.T { t; interface }) f =
  let module M =
    (val Provider.Interface.lookup interface ~trait:Provider_interface.Float_printer)
  in
  print_endline (M.string_of_float t f)
;;

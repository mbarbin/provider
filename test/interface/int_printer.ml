type tag = [ `Int_printer ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val string_of_int : t -> int -> string
  end

  type (_, _, _) Provider.Trait.t +=
    | Int_printer : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t

  let make (type t) (module M : S with type t = t) =
    Provider.Interface.make [ Provider.Trait.implement Int_printer ~impl:(module M) ]
  ;;
end

let print (Provider.T { t; interface }) i =
  let module M =
    (val Provider.Interface.lookup interface ~trait:Provider_interface.Int_printer)
  in
  print_endline (M.string_of_int t i)
;;

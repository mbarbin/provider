type tag = [ `Float_printer ]
type 'a t = ([> tag ] as 'a) Provider.packed

module Provider_interface = struct
  module type S = sig
    type t

    val string_of_float : t -> float -> string
  end

  module Trait = Provider.Trait.Create (struct
      type 'a module_type = (module S with type t = 'a)
    end)

  let float_printer =
    (Trait.t : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t)
  ;;

  let () = Provider.Trait.Info.register_name float_printer ~name:"Float_printer"

  let make (type t) (module M : S with type t = t) =
    Provider.make [ Provider.implement float_printer ~impl:(module M) ]
  ;;
end

let print (Provider.T { t; provider }) f =
  let module M = (val Provider.lookup provider ~trait:Provider_interface.float_printer) in
  Stdlib.print_endline (M.string_of_float t f)
;;

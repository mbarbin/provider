type tag = [ `Float_printer ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val string_of_float : t -> float -> string
  end

  module Trait = struct
    type (_, _, _) Provider.Trait.t +=
      | Float_printer : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t

    let t = Float_printer
  end

  let float_printer =
    (Trait.t : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t)
  ;;

  let () = Provider.Trait.Info.register_name float_printer ~name:"Float_printer"

  let make (type t) (module M : S with type t = t) =
    Provider.Handler.make [ Provider.Trait.implement float_printer ~impl:(module M) ]
  ;;
end

let print (Provider.T { t; handler }) f =
  let module M =
    (val Provider.Handler.lookup handler ~trait:Provider_interface.float_printer)
  in
  Stdlib.print_endline (M.string_of_float t f)
;;

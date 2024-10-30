type tag = [ `Int_printer ]
type 'a t = ([> tag ] as 'a) Provider.t

module Provider_interface = struct
  module type S = sig
    type t

    val string_of_int : t -> int -> string
  end

  module Trait = struct
    type (_, _, _) Provider.Trait.t +=
      | Int_printer : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t

    let t = Int_printer
  end

  let int_printer =
    (Trait.t : ('t, (module S with type t = 't), [> tag ]) Provider.Trait.t)
  ;;

  let () = Provider.Trait.Info.register_name int_printer ~name:"Int_printer"

  let make (type t) (module M : S with type t = t) =
    Provider.Handler.make [ Provider.Trait.implement int_printer ~impl:(module M) ]
  ;;
end

let print (Provider.T { t; handler }) i =
  let module M =
    (val Provider.Handler.lookup handler ~trait:Provider_interface.int_printer)
  in
  Stdlib.print_endline (M.string_of_int t i)
;;

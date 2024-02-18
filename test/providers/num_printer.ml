module Impl = struct
  type t = unit

  let string_of_int () i = Int.to_string i
  let string_of_float () f = Float.to_string f
end

include Impl

let interface : (unit, [ `Int_printer | `Float_printer ]) Provider.Interface.t =
  Provider.Interface.make
    (List.concat
       [ Interface.Int_printer.Provider_interface.make (module Impl)
         |> Provider.Interface.classes
       ; Interface.Float_printer.Provider_interface.make (module Impl)
         |> Provider.Interface.classes
       ])
;;

let make () : [ `Int_printer | `Float_printer ] Provider.t =
  Provider.T { t = (); interface }
;;

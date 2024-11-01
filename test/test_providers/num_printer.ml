module Impl = struct
  type t = unit

  let string_of_int () i = Int.to_string i
  let string_of_float () f = Float.to_string f
end

include Impl

let provider : (unit, [ `Int_printer | `Float_printer ]) Provider.t =
  Provider.make
    (List.concat
       [ Test_interfaces.Int_printer.Provider_interface.make (module Impl)
         |> Provider.bindings
       ; Test_interfaces.Float_printer.Provider_interface.make (module Impl)
         |> Provider.bindings
       ])
;;

let make () : [ `Int_printer | `Float_printer ] Provider.packed =
  Provider.T { t = (); provider }
;;

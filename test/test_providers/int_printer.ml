module Impl = struct
  type t = unit

  let string_of_int () i = Int.to_string i
end

include Impl

let provider : (unit, [ `Int_printer ]) Provider.t =
  Provider.make
    (List.concat
       [ Test_interfaces.Int_printer.Provider_interface.make (module Impl)
         |> Provider.bindings
       ])
;;

let make () : [ `Int_printer ] Provider.packed = Provider.T { t = (); provider }

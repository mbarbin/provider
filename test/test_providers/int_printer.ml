module Impl = struct
  type t = unit

  let string_of_int () i = Int.to_string i
end

include Impl

let handler : (unit, [ `Int_printer ]) Provider.Handler.t =
  Provider.Handler.make
    (List.concat
       [ Test_interfaces.Int_printer.Provider_interface.make (module Impl)
         |> Provider.Handler.bindings
       ])
;;

let make () : [ `Int_printer ] Provider.t = Provider.T { t = (); handler }

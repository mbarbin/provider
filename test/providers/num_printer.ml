module Impl = struct
  type t = unit

  let string_of_int () i = Int.to_string i
  let string_of_float () f = Float.to_string f
end

include Impl

let handler : (unit, [ `Int_printer | `Float_printer ]) Provider.Handler.t =
  Provider.Handler.make
    (List.concat
       [ Interface.Int_printer.Provider_interface.make (module Impl)
         |> Provider.Handler.implementations
       ; Interface.Float_printer.Provider_interface.make (module Impl)
         |> Provider.Handler.implementations
       ])
;;

let make () : [ `Int_printer | `Float_printer ] Provider.t =
  Provider.T { t = (); handler }
;;

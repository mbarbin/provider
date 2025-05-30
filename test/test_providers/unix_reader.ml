module Impl = struct
  type t = unit

  let readdir () ~path =
    Array.to_list (Stdlib.Sys.readdir path) |> List.sort ~compare:String.compare
  ;;
end

include Impl

let make () : [ `Directory_reader ] Provider.packed =
  Provider.T
    { t = ()
    ; provider = Test_interfaces.Directory_reader.Provider_interface.make (module Impl)
    }
;;

module Impl = struct
  type t = unit

  let readdir () ~path =
    Array.to_list (Stdlib.Sys.readdir path) |> List.sort ~compare:String.compare
  ;;
end

include Impl

let make () : [ `Directory_reader ] Provider.t =
  Provider.T
    { t = ()
    ; interface =
        Provider.Interface.make
          [ Provider.Class.implement
              ~class_id:Interface.Directory_reader.Provider_interface.Directory_reader
              (module Impl)
          ]
    }
;;

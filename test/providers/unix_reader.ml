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
    ; handler =
        Provider.Handler.make
          [ Provider.Trait.implement
              Interface.Directory_reader.Provider_interface.Directory_reader
              ~impl:(module Impl)
          ]
    }
;;

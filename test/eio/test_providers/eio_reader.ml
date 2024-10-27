module Impl = struct
  type t = T : { fs : _ Eio.Path.t } -> t

  let readdir (T { fs }) ~path =
    let path = Eio.Path.(fs / path) in
    Eio.Path.read_dir path
  ;;

  let load (T { fs }) ~path = Eio.Path.load Eio.Path.(fs / path)
end

include Impl

let make ~env : [ `Directory_reader | `File_reader ] Provider.t =
  Provider.T
    { t = Impl.T { fs = Eio.Stdenv.fs env }
    ; handler =
        Provider.Handler.make
          [ Provider.Trait.implement
              Test_interfaces.Directory_reader.Provider_interface.Directory_reader
              ~impl:(module Impl)
          ; Provider.Trait.implement
              Test_interfaces.File_reader.Provider_interface.File_reader
              ~impl:(module Impl)
          ]
    }
;;

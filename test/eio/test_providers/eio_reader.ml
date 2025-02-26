module Impl = struct
  type t = T : { fs : _ Eio.Path.t } -> t

  let readdir (T { fs }) ~path =
    let path = Eio.Path.(fs / path) in
    Eio.Path.read_dir path
  ;;

  let load (T { fs }) ~path = Eio.Path.load Eio.Path.(fs / path)
end

include Impl

let make ~env : [ `Directory_reader | `File_reader ] Provider.packed =
  Provider.T
    { t = Impl.T { fs = Eio.Stdenv.fs env }
    ; provider =
        Provider.make
          [ Provider.implement
              Test_interfaces.Directory_reader.Provider_interface.directory_reader
              ~impl:(module Impl)
          ; Provider.implement
              Test_interfaces.File_reader.Provider_interface.file_reader
              ~impl:(module Impl)
          ]
    }
;;

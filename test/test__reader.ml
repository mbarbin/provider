(* This test demonstrates the use of two interfaces:

   1. An interface for listing the files present in a directory, and
   2. An interface for accessing the contents of a given file.

   We have provided two implementations for each interface, one based on [Eio]
   and one based on [Stdlib.Unix].

   The test showcases how to create the parametrized interfaces, instantiate
   them with the actual implementations, and how to call them. It also includes
   a case of dynamic lookup, demonstrating how you can express behavior that
   depends on the implementations available in the object provided. *)

let with_temp_dir ~env ~path ~f =
  let cwd = Unix.getcwd () in
  let dir = Eio.Path.(Eio.Stdenv.fs env / cwd / path) in
  Exn.protect
    ~f:(fun () ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o777 dir;
      f (snd dir))
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
;;

(* This function requires the [Directory_reader] capability. *)
let print_all_text_files t ~path =
  print_s
    [%sexp
      (Interface.Directory_reader.find_files_with_extension t ~path ~ext:".txt"
       : string list)]
;;

(* This function requires both [Directory_reader] and [File_reader]
   capabilities. *)
let print_all_text_files_with_lines t ~path =
  List.iter
    (Interface.Directory_reader.find_files_with_extension t ~path ~ext:".txt")
    ~f:(fun file ->
      let lines =
        let contents = Interface.File_reader.load t ~path:(path ^ "/" ^ file) in
        List.sum (module Int) (String.split_lines contents) ~f:(Fn.const 1)
      in
      print_s [%sexp { file : string; lines : int }])
;;

(* This is an example of a function that requires the [Directory_reader]
   capability and use the [File_reader] capability if available, but without
   requiring it. *)
let print_all_text_files_with_lines_if_available t ~path =
  List.iter
    (Interface.Directory_reader.find_files_with_extension t ~path ~ext:".txt")
    ~f:(fun file ->
      let lines =
        let (Provider.T { t; interface }) = t in
        match
          Provider.Interface.lookup_opt
            interface
            ~trait:Interface.File_reader.Provider_interface.File_reader
        with
        | None -> "not-available"
        | Some (module File_reader) ->
          let contents = File_reader.load t ~path:(path ^ "/" ^ file) in
          List.sum (module Int) (String.split_lines contents) ~f:(Fn.const 1)
          |> Int.to_string_hum
      in
      print_s [%sexp { file : string; lines : string }])
;;

(* Now let's put it all together in a test. *)
let%expect_test "test" =
  let unix_reader = Providers.Unix_reader.make () in
  Eio_main.run
  @@ fun env ->
  let eio_reader = Providers.Eio_reader.make ~env in
  with_temp_dir ~env ~path:"test" ~f:(fun dir ->
    print_s
      [%sexp (Interface.Directory_reader.readdir unix_reader ~path:dir : string list)];
    [%expect {| () |}];
    print_s
      [%sexp (Interface.Directory_reader.readdir eio_reader ~path:dir : string list)];
    [%expect {| () |}];
    print_all_text_files unix_reader ~path:dir;
    [%expect {| () |}];
    print_all_text_files eio_reader ~path:dir;
    [%expect {| () |}];
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / dir / "a.txt")
      (String.strip {|
Hello file a
With multiple lines
|});
    Eio.Path.save
      ~create:(`Or_truncate 0o600)
      Eio.Path.(Eio.Stdenv.fs env / dir / "b.txt")
      (String.strip {|
Hello file b
With even more
 lines
|});
    print_all_text_files unix_reader ~path:dir;
    [%expect {| (a.txt b.txt) |}];
    print_all_text_files eio_reader ~path:dir;
    [%expect {| (a.txt b.txt) |}];
    print_all_text_files_with_lines eio_reader ~path:dir;
    [%expect {|
    ((file  a.txt)
     (lines 2))
    ((file  b.txt)
     (lines 3)) |}];
    print_all_text_files_with_lines_if_available unix_reader ~path:dir;
    [%expect
      {|
      ((file  a.txt)
       (lines not-available))
      ((file  b.txt)
       (lines not-available)) |}];
    print_all_text_files_with_lines_if_available eio_reader ~path:dir;
    [%expect
      {|
      ((file  a.txt)
       (lines 2))
      ((file  b.txt)
       (lines 3)) |}];
    ())
;;

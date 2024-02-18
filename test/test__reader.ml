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

(* This test is focused on the "build class" functionality. We are testing that
   different ways to create an interface - using 'make', 'extend', 'class
   implement', or the provider interface supplied maker - all result in equivalent
   interfaces. This ensures consistency across different methods of interface
   creation. *)
let%expect_test "build class" =
  let class1 =
    Provider.Class.implement
      ~class_id:Interface.Directory_reader.Provider_interface.Directory_reader
      (module Providers.Eio_reader.Impl)
  in
  let eio1 =
    Interface.Directory_reader.Provider_interface.make (module Providers.Eio_reader.Impl)
  in
  (match class1, List.hd_exn (Provider.Interface.classes eio1) with
   | T t, T t' ->
     require [%here] (Provider.Class_id.same t.class_id t'.class_id);
     [%expect {||}];
     ());
  let class2 =
    Provider.Class.implement
      ~class_id:Interface.File_reader.Provider_interface.File_reader
      (module Providers.Eio_reader.Impl)
  in
  (match class1, class2 with
   | T t1, T t2 ->
     print_s
       [%sexp
         { class1 = (Provider.Class_id.info t1.class_id : Provider.Class_id.Info.t)
         ; class2 = (Provider.Class_id.info t2.class_id : Provider.Class_id.Info.t)
         }];
     [%expect
       {|
       ((class1 (
          (id #id)
          (name
           Provider_test__Interface__Directory_reader.Provider_interface.Directory_reader)))
        (class2 (
          (id #id)
          (name Provider_test__Interface__File_reader.Provider_interface.File_reader)))) |}];
     require [%here] (not (Provider.Class_id.same t1.class_id t2.class_id));
     [%expect {||}];
     ());
  (match Provider.Interface.classes eio1 with
   | [ c1 ] ->
     require_equal
       [%here]
       (module Provider.Class_id.Uid)
       (Provider.Class.uid c1)
       (Provider.Class.uid class1);
     [%expect {||}]
   | _ -> assert false);
  let empty = Provider.Interface.make [] in
  require [%here] (Provider.Interface.is_empty empty);
  let eio2 = Provider.Interface.make [ class2 ] in
  require [%here] (not (Provider.Interface.is_empty eio2));
  let eio3 = Provider.Interface.make [ class1; class2 ] in
  let eio4 = Provider.Interface.extend eio1 ~with_:(Provider.Interface.classes eio2) in
  require [%here] (Provider.Private.Interface.same_class_uids eio3 eio4);
  [%expect {||}];
  ()
;;

let print_implemented_classes (Provider.T { t = _; interface }) =
  let info =
    List.map (Provider.Interface.classes interface) ~f:(fun class_ ->
      [%sexp (Provider.Class.info class_ : Provider.Class_id.Info.t)])
  in
  print_s [%sexp (info : Sexp.t list)]
;;

let print_implements (Provider.T { t = _; interface }) =
  let implements class_id = Provider.Interface.implements interface ~class_id in
  print_s
    [%sexp
      { implements =
          { file_reader =
              (implements Interface.File_reader.Provider_interface.File_reader : bool)
          ; directory_reader =
              (implements Interface.Directory_reader.Provider_interface.Directory_reader
               : bool)
          }
      }]
;;

(* This test demonstrates how to access information about the classes
   implemented by a provider at runtime. This is a key aspect of introspection,
   allowing you to understand the capabilities of a provider dynamically, as the
   program is running. *)
let%expect_test "introspection" =
  let unix_reader = Providers.Unix_reader.make () in
  Eio_main.run
  @@ fun env ->
  let eio_reader = Providers.Eio_reader.make ~env in
  print_implements eio_reader;
  [%expect
    {|
    ((
      implements (
        (file_reader      true)
        (directory_reader true)))) |}];
  print_implements unix_reader;
  [%expect
    {|
    ((
      implements (
        (file_reader      false)
        (directory_reader true)))) |}];
  let id_mapping = Hashtbl.create (module Int) in
  let next_id = ref 0 in
  let sexp_of_id id =
    let id =
      match Hashtbl.find id_mapping id with
      | Some id -> id
      | None ->
        let data = !next_id in
        Int.incr next_id;
        Hashtbl.set id_mapping ~key:id ~data;
        data
    in
    Sexp.Atom (Int.to_string id)
  in
  Ref.set_temporarily Provider.Class_id.Info.sexp_of_id sexp_of_id ~f:(fun () ->
    print_implemented_classes unix_reader;
    [%expect
      {|
      ((
        (id 0)
        (name
         Provider_test__Interface__Directory_reader.Provider_interface.Directory_reader))) |}];
    print_implemented_classes eio_reader;
    [%expect
      {|
      (((id 0)
        (name
         Provider_test__Interface__Directory_reader.Provider_interface.Directory_reader))
       ((id 1)
        (name Provider_test__Interface__File_reader.Provider_interface.File_reader))) |}];
    ());
  ()
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
            ~class_id:Interface.File_reader.Provider_interface.File_reader
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

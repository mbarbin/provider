(* This test demonstrates how to access information about the traits implemented
   by a provider at runtime. This is a key aspect of introspection, allowing you
   to understand the capabilities of a provider dynamically, as the program is
   running. *)

let print_implemented_traits (Provider.T { t = _; interface }) =
  let info =
    List.map (Provider.Interface.implementations interface) ~f:(fun implementation ->
      [%sexp (Provider.Implementation.info implementation : Provider.Trait.Info.t)])
  in
  print_s [%sexp (info : Sexp.t list)]
;;

let print_implements (Provider.T { t = _; interface }) =
  let implements trait = Provider.Interface.implements interface ~trait in
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

let%expect_test "introspection" =
  print_implements (Provider.T { t = (); interface = Provider.Interface.make [] });
  [%expect
    {|
    ((
      implements (
        (file_reader      false)
        (directory_reader false)))) |}];
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
  Ref.set_temporarily Provider.Trait.Info.sexp_of_id sexp_of_id ~f:(fun () ->
    print_implemented_traits unix_reader;
    [%expect
      {|
      ((
        (id 0)
        (name
         Provider_test__Interface__Directory_reader.Provider_interface.Directory_reader))) |}];
    print_implemented_traits eio_reader;
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

(* This test demonstrates how to access information about the traits implemented
   by a provider at runtime. This is a key aspect of introspection, allowing you
   to understand the capabilities of a provider dynamically, as the program is
   running. *)

let print_implemented_traits (Provider.T { t = _; handler }) =
  let info =
    List.map (Provider.Handler.bindings handler) ~f:(fun binding ->
      [%sexp (Provider.Binding.info binding : Provider.Trait.Info.t)])
  in
  print_s [%sexp (info : Sexp.t list)]
;;

let print_implements (Provider.T { t = _; handler }) =
  let implements trait = Provider.Handler.implements handler ~trait in
  print_s
    [%sexp
      { implements =
          { file_reader =
              (implements Interface.File_reader.Provider_interface.File_reader : bool)
          ; directory_reader =
              (implements Interface.Directory_reader.Provider_interface.Directory_reader
               : bool)
          ; int_printer =
              (implements Interface.Int_printer.Provider_interface.Int_printer : bool)
          }
      }]
;;

let%expect_test "introspection" =
  print_implements (Provider.T { t = (); handler = Provider.Handler.make [] });
  [%expect
    {|
    ((
      implements (
        (file_reader      false)
        (directory_reader false)
        (int_printer      false))))
    |}];
  let unix_reader = Providers.Unix_reader.make () in
  let num_printer = Providers.Num_printer.make () in
  print_implements num_printer;
  [%expect
    {|
    ((
      implements (
        (file_reader      false)
        (directory_reader false)
        (int_printer      true))))
    |}];
  print_implements unix_reader;
  [%expect
    {|
    ((
      implements (
        (file_reader      false)
        (directory_reader true)
        (int_printer      false))))
    |}];
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
    print_implemented_traits num_printer;
    [%expect
      {|
      (((id 1)
        (name
         Provider_test__Interface__Float_printer.Provider_interface.Float_printer))
       ((id 2)
        (name Provider_test__Interface__Int_printer.Provider_interface.Int_printer)))
      |}];
    ());
  ()
;;

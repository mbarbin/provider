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
              (implements Test_interfaces.File_reader.Provider_interface.File_reader
               : bool)
          ; directory_reader =
              (implements
                 Test_interfaces.Directory_reader.Provider_interface.Directory_reader
               : bool)
          ; int_printer =
              (implements Test_interfaces.Int_printer.Provider_interface.Int_printer
               : bool)
          ; float_printer =
              (implements Test_interfaces.Float_printer.Provider_interface.Float_printer
               : bool)
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
        (int_printer      false)
        (float_printer    false))))
    |}];
  let int_printer = Test_providers.Int_printer.make () in
  let num_printer = Test_providers.Num_printer.make () in
  print_implements num_printer;
  [%expect
    {|
    ((
      implements (
        (file_reader      false)
        (directory_reader false)
        (int_printer      true)
        (float_printer    true))))
    |}];
  print_implements int_printer;
  [%expect
    {|
    ((
      implements (
        (file_reader      false)
        (directory_reader false)
        (int_printer      true)
        (float_printer    false))))
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
    print_implemented_traits int_printer;
    [%expect {|
      ((
        (id   0)
        (name Int_printer)))
      |}];
    print_implemented_traits num_printer;
    [%expect
      {|
      (((id 0) (name Int_printer))
       ((id 1) (name Float_printer)))
      |}];
    ());
  ()
;;

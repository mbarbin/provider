(* Here we verify that it is possible to override an existing trait. *)

module Int_hum_printer = struct
  module Impl = struct
    type t = unit

    let string_of_int () i = Int.to_string_hum i
  end

  let make () : [ `Int_printer | `Float_printer ] Provider.t =
    Provider.T
      { t = ()
      ; interface =
          Provider.Interface.extend
            Providers.Num_printer.interface
            ~with_:
              [ Provider.Trait.implement
                  Interface.Int_printer.Provider_interface.Int_printer
                  ~impl:(module Impl)
              ]
      }
  ;;
end

let%expect_test "override" =
  let print_implemented_traits (Provider.T { t = _; interface }) =
    let info =
      List.map (Provider.Interface.implementations interface) ~f:(fun implementation ->
        [%sexp (Provider.Binding.info implementation : Provider.Trait.Info.t)])
    in
    print_s [%sexp (info : Sexp.t list)]
  in
  let test printer =
    Interface.Int_printer.print printer 1234;
    Interface.Float_printer.print printer 1234.5678
  in
  let num_printer = Providers.Num_printer.make () in
  print_implemented_traits num_printer;
  [%expect
    {|
      (((id #id)
        (name
         Provider_test__Interface__Float_printer.Provider_interface.Float_printer))
       ((id #id)
        (name Provider_test__Interface__Int_printer.Provider_interface.Int_printer))) |}];
  test num_printer;
  [%expect {|
      1234
      1234.5678 |}];
  let hum_printer = Int_hum_printer.make () in
  print_implemented_traits hum_printer;
  [%expect
    {|
      (((id #id)
        (name
         Provider_test__Interface__Float_printer.Provider_interface.Float_printer))
       ((id #id)
        (name Provider_test__Interface__Int_printer.Provider_interface.Int_printer))) |}];
  test hum_printer;
  (* Now there's an additional underscore separator in '1_234'. *)
  [%expect {|
      1_234
      1234.5678 |}];
  ()
;;

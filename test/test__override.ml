(* In the next part of the test, we verify that it is possible to override an
   existing class. *)

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
              [ Provider.Class.implement
                  ~class_id:Interface.Int_printer.Provider_interface.Int_printer
                  (module Impl)
              ]
      }
  ;;
end

let%expect_test "override" =
  let print_implemented_classes (Provider.T { t = _; interface }) =
    let info =
      List.map (Provider.Interface.classes interface) ~f:(fun class_ ->
        [%sexp (Provider.Class.info class_ : Provider.Class_id.Info.t)])
    in
    print_s [%sexp (info : Sexp.t list)]
  in
  let test printer =
    Interface.Int_printer.print printer 1234;
    Interface.Float_printer.print printer 1234.5678
  in
  let num_printer = Providers.Num_printer.make () in
  print_implemented_classes num_printer;
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
  print_implemented_classes hum_printer;
  [%expect
    {|
      (((id #id)
        (name
         Provider_test__Interface__Float_printer.Provider_interface.Float_printer))
       ((id #id)
        (name Provider_test__Interface__Int_printer.Provider_interface.Int_printer))) |}];
  test hum_printer;
  [%expect {|
      1_234
      1234.5678 |}];
  ()
;;

(* Here we verify that it is possible to override an existing trait. *)

module Int_hum_printer = struct
  module Impl = struct
    type t = unit

    let string_of_int () i = Int.to_string_hum i
  end

  let make () : [ `Int_printer | `Float_printer ] Provider.t =
    Provider.T
      { t = ()
      ; handler =
          Provider.Handler.extend
            Test_providers.Num_printer.handler
            ~with_:
              [ Provider.Trait.implement
                  Test_interfaces.Int_printer.Provider_interface.int_printer
                  ~impl:(module Impl)
              ]
      }
  ;;
end

let%expect_test "override" =
  let print_implemented_traits (Provider.T { t = _; handler }) =
    let info =
      List.map (Provider.Handler.bindings handler) ~f:(fun binding ->
        [%sexp (Provider.Binding.info binding : Provider.Trait.Info.t)])
    in
    print_s [%sexp (info : Sexp.t list)]
  in
  let test printer =
    Test_interfaces.Int_printer.print printer 1234;
    Test_interfaces.Float_printer.print printer 1234.5678
  in
  let num_printer = Test_providers.Num_printer.make () in
  print_implemented_traits num_printer;
  [%expect
    {|
    (((id #id) (name Int_printer))
     ((id #id) (name Float_printer)))
    |}];
  test num_printer;
  [%expect {|
      1234
      1234.5678 |}];
  let hum_printer = Int_hum_printer.make () in
  print_implemented_traits hum_printer;
  [%expect
    {|
    (((id #id) (name Int_printer))
     ((id #id) (name Float_printer)))
    |}];
  test hum_printer;
  (* Now there's an additional underscore separator in '1_234'. *)
  [%expect {|
      1_234
      1234.5678 |}];
  ()
;;

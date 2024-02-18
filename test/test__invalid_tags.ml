(* This is the responsibility of the provider to correctly create interfaces of
   the right types. A programming error may lead to an invalid lookup at
   runtime. *)

let%expect_test "invalid tags" =
  (* [Int_printer] was correctly built. *)
  let print_42 printer = Int_printer.print printer 42 in
  print_42 (Providers.Num_printer.make ());
  [%expect {| 42 |}];
  (* Now let's build a provider with an empty interface, that claims however to
     implement the [Int_printer] interface. *)
  let module Invalid_int_printer = struct
    let make () : [ `Int_printer ] Provider.t =
      Provider.T { t = (); interface = Provider.Interface.make [] }
    ;;
  end
  in
  require_does_raise [%here] (fun () -> print_42 (Invalid_int_printer.make ()));
  [%expect
    {|
    ("Class not implemented" ((
      class_info (
        (id #id) (name Provider_test.Int_printer.Provider_interface.Int_printer))))) |}];
  ()
;;

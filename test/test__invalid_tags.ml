(* This is the responsibility of the provider to correctly create interfaces of
   the right types. A programming error may lead to an invalid lookup at
   runtime. *)

let%expect_test "invalid tags" =
  (* [Test_providers.Num_printer] was correctly built. *)
  let print_42 printer = Test_interfaces.Int_printer.print printer 42 in
  print_42 (Test_providers.Num_printer.make ());
  [%expect {| 42 |}];
  (* Now let's build a provider with an empty interface, that claims however to
     implement the [Int_printer] interface. *)
  let module Invalid_int_printer = struct
    let make () : [ `Int_printer ] Provider.t =
      Provider.T { t = (); handler = Provider.Handler.make [] }
    ;;
  end
  in
  require_does_raise [%here] (fun () -> print_42 (Invalid_int_printer.make ()));
  [%expect
    {|
    ("Trait not implemented" ((
      trait_info (
        (id   #id)
        (name Int_printer)))))
    |}];
  ()
;;

(* This test is focused on the "make interface" functionality. We are testing
   that different ways to create an interface -- using [make], [extend], [Trait.
   implement], or the provider interface supplied maker -- all result in
   equivalent interfaces. This ensures consistency across different methods of
   interface creation. *)

let%expect_test "int-printer" =
  let printer = Test_providers.Int_printer.make () in
  Test_interfaces.Int_printer.print printer 123_456_789;
  [%expect {| 123456789 |}];
  ()
;;

let%expect_test "make interface" =
  let binding1 =
    Provider.Trait.implement
      Test_interfaces.Int_printer.Provider_interface.Int_printer
      ~impl:(module Test_providers.Num_printer.Impl)
  in
  Test_interfaces.Int_printer.print
    (Provider.T { t = (); handler = Provider.Handler.make [ binding1 ] })
    1234;
  [%expect {| 1234 |}];
  let num1 =
    Test_interfaces.Int_printer.Provider_interface.make
      (module Test_providers.Num_printer.Impl)
  in
  Test_interfaces.Int_printer.print (Provider.T { t = (); handler = num1 }) 5678;
  [%expect {| 5678 |}];
  (match binding1, List.hd_exn (Provider.Handler.bindings num1) with
   | T t, T t' ->
     require [%here] (Provider.Trait.same t.trait t'.trait);
     [%expect {||}];
     ());
  let binding2 =
    Provider.Trait.implement
      Test_interfaces.Float_printer.Provider_interface.Float_printer
      ~impl:(module Test_providers.Num_printer.Impl)
  in
  (match binding1, binding2 with
   | T t1, T t2 ->
     print_s
       [%sexp
         { trait1 = (Provider.Trait.info t1.trait : Provider.Trait.Info.t)
         ; trait2 = (Provider.Trait.info t2.trait : Provider.Trait.Info.t)
         }];
     [%expect
       {|
       ((trait1 ((id #id) (name Int_printer)))
        (trait2 ((id #id) (name Float_printer))))
       |}];
     require [%here] (not (Provider.Trait.same t1.trait t2.trait));
     [%expect {||}];
     ());
  (match Provider.Handler.bindings num1 with
   | [ c1 ] ->
     require_equal
       [%here]
       (module Provider.Trait.Uid)
       (Provider.Binding.uid c1)
       (Provider.Binding.uid binding1);
     [%expect {||}]
   | _ -> assert false);
  let empty = Provider.Handler.make [] in
  require [%here] (Provider.Handler.is_empty empty);
  require [%here] (List.is_empty (Provider.Handler.bindings empty));
  let num2 = Provider.Handler.make [ binding2 ] in
  require [%here] (not (Provider.Handler.is_empty num2));
  require [%here] (not (Provider.Private.Handler.same_trait_uids empty num2));
  [%expect {||}];
  let num3 = Provider.Handler.make [ binding1; binding2 ] in
  let num4 = Provider.Handler.extend num1 ~with_:(Provider.Handler.bindings num2) in
  require [%here] (Provider.Private.Handler.same_trait_uids num3 num4);
  [%expect {||}];
  ()
;;

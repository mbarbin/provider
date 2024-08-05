(* This test is focused on the "make interface" functionality. We are testing
   that different ways to create an interface -- using [make], [extend], [Trait.
   implement], or the provider interface supplied maker -- all result in
   equivalent interfaces. This ensures consistency across different methods of
   interface creation. *)

let%expect_test "make interface" =
  let trait1 =
    Provider.Trait.implement
      Interface.Directory_reader.Provider_interface.Directory_reader
      ~impl:(module Providers.Eio_reader.Impl)
  in
  let eio1 =
    Interface.Directory_reader.Provider_interface.make (module Providers.Eio_reader.Impl)
  in
  (match trait1, List.hd_exn (Provider.Handler.implementations eio1) with
   | T t, T t' ->
     require [%here] (Provider.Trait.same t.trait t'.trait);
     [%expect {||}];
     ());
  let trait2 =
    Provider.Trait.implement
      Interface.File_reader.Provider_interface.File_reader
      ~impl:(module Providers.Eio_reader.Impl)
  in
  (match trait1, trait2 with
   | T t1, T t2 ->
     print_s
       [%sexp
         { trait1 = (Provider.Trait.info t1.trait : Provider.Trait.Info.t)
         ; trait2 = (Provider.Trait.info t2.trait : Provider.Trait.Info.t)
         }];
     [%expect
       {|
        ((trait1 (
           (id #id)
           (name
            Provider_test__Interface__Directory_reader.Provider_interface.Directory_reader)))
         (trait2 (
           (id #id)
           (name Provider_test__Interface__File_reader.Provider_interface.File_reader)))) |}];
     require [%here] (not (Provider.Trait.same t1.trait t2.trait));
     [%expect {||}];
     ());
  (match Provider.Handler.implementations eio1 with
   | [ c1 ] ->
     require_equal
       [%here]
       (module Provider.Trait.Uid)
       (Provider.Binding.uid c1)
       (Provider.Binding.uid trait1);
     [%expect {||}]
   | _ -> assert false);
  let empty = Provider.Handler.make [] in
  require [%here] (Provider.Handler.is_empty empty);
  require [%here] (List.is_empty (Provider.Handler.implementations empty));
  let eio2 = Provider.Handler.make [ trait2 ] in
  require [%here] (not (Provider.Handler.is_empty eio2));
  require [%here] (not (Provider.Private.Handler.same_trait_uids empty eio2));
  [%expect {||}];
  let eio3 = Provider.Handler.make [ trait1; trait2 ] in
  let eio4 =
    Provider.Handler.extend eio1 ~with_:(Provider.Handler.implementations eio2)
  in
  require [%here] (Provider.Private.Handler.same_trait_uids eio3 eio4);
  [%expect {||}];
  ()
;;

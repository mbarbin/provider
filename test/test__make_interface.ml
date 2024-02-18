(* This test is focused on the "make interface" functionality. We are testing
   that different ways to create an interface - using 'make', 'extend', 'class
   implement', or the provider interface supplied maker - all result in
   equivalent interfaces. This ensures consistency across different methods of
   interface creation. *)

let%expect_test "make interface" =
  let class1 =
    Provider.Class.implement
      ~class_id:Interface.Directory_reader.Provider_interface.Directory_reader
      (module Providers.Eio_reader.Impl)
  in
  let eio1 =
    Interface.Directory_reader.Provider_interface.make (module Providers.Eio_reader.Impl)
  in
  (match class1, List.hd_exn (Provider.Interface.classes eio1) with
   | T t, T t' ->
     require [%here] (Provider.Class_id.same t.class_id t'.class_id);
     [%expect {||}];
     ());
  let class2 =
    Provider.Class.implement
      ~class_id:Interface.File_reader.Provider_interface.File_reader
      (module Providers.Eio_reader.Impl)
  in
  (match class1, class2 with
   | T t1, T t2 ->
     print_s
       [%sexp
         { class1 = (Provider.Class_id.info t1.class_id : Provider.Class_id.Info.t)
         ; class2 = (Provider.Class_id.info t2.class_id : Provider.Class_id.Info.t)
         }];
     [%expect
       {|
        ((class1 (
           (id #id)
           (name
            Provider_test__Interface__Directory_reader.Provider_interface.Directory_reader)))
         (class2 (
           (id #id)
           (name Provider_test__Interface__File_reader.Provider_interface.File_reader)))) |}];
     require [%here] (not (Provider.Class_id.same t1.class_id t2.class_id));
     [%expect {||}];
     ());
  (match Provider.Interface.classes eio1 with
   | [ c1 ] ->
     require_equal
       [%here]
       (module Provider.Class_id.Uid)
       (Provider.Class.uid c1)
       (Provider.Class.uid class1);
     [%expect {||}]
   | _ -> assert false);
  let empty = Provider.Interface.make [] in
  require [%here] (Provider.Interface.is_empty empty);
  require [%here] (List.is_empty (Provider.Interface.classes empty));
  let eio2 = Provider.Interface.make [ class2 ] in
  require [%here] (not (Provider.Interface.is_empty eio2));
  require [%here] (not (Provider.Private.Interface.same_class_uids empty eio2));
  [%expect {||}];
  let eio3 = Provider.Interface.make [ class1; class2 ] in
  let eio4 = Provider.Interface.extend eio1 ~with_:(Provider.Interface.classes eio2) in
  require [%here] (Provider.Private.Interface.same_class_uids eio3 eio4);
  [%expect {||}];
  ()
;;

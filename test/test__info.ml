module T : sig
  val t : ('t, (module T with type t = 't), [> `T ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 't module_type = (module T with type t = 't)
  end)

let%expect_test "info" =
  (* By default, id are not shown, and trait do not have names. *)
  let print_info () = print_s [%sexp (Provider.Trait.info T.t : Provider.Trait.Info.t)] in
  [%expect {||}];
  (* It is possible to show the id with custom functions. *)
  Ref.set_temporarily
    Provider.Trait.Info.sexp_of_id
    (fun (_ : int) -> Sexp.Atom "#customized-id")
    ~f:(fun () -> print_info ());
  [%expect
    {|
    ((id   #customized-id)
     (name <none>))
    |}];
  (* It is also possible to register a name for a trait. *)
  let () = Provider.Trait.Info.register_name T.t ~name:"Hello Name!" in
  print_info ();
  [%expect
    {|
    ((id   #id)
     (name "Hello Name!"))
    |}];
  (* The name can be changed. Whether this is desirable is up to the user. *)
  let () = Provider.Trait.Info.register_name T.t ~name:"Goodbye Name!" in
  print_info ();
  [%expect
    {|
    ((id   #id)
     (name "Goodbye Name!"))
    |}];
  ()
;;

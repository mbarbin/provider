(* The class lookup implementation uses a cache to speed up the search when
   looking up the same class over and over in a row. In this test we monitor how
   the cache is updated. *)

module Cache_state = struct
  type t =
    | None
    | Int_printer
    | Float_printer
  [@@deriving equal, sexp_of]
end

let%expect_test "override" =
  let num_printer = Providers.Num_printer.make () in
  let cache_state_of_uid uid =
    if Provider.Class_id.Uid.equal
         uid
         (Interface.Int_printer.Provider_interface.Int_printer |> Provider.Class_id.uid)
    then Cache_state.Int_printer
    else if Provider.Class_id.Uid.equal
              uid
              (Interface.Float_printer.Provider_interface.Float_printer
               |> Provider.Class_id.uid)
    then Cache_state.Float_printer
    else assert false
  in
  let cache_state interface =
    match Provider.Private.Interface.cache interface with
    | None -> Cache_state.None
    | Some uid -> cache_state_of_uid uid
  in
  let show_cache interface = print_s [%sexp (cache_state interface : Cache_state.t)] in
  let show_printer_cache () =
    let (Provider.T { t = _; interface }) = num_printer in
    show_cache interface
  in
  (* An empty interface has no cache. *)
  show_cache (Provider.Interface.make []);
  [%expect {| None |}];
  let (Provider.T { t = _; interface }) = num_printer in
  let int_printer_lookup () =
    (fun (type a) (interface : (a, _) Provider.Interface.t) ->
      ignore
        (Provider.Interface.lookup
           interface
           ~class_id:Interface.Int_printer.Provider_interface.Int_printer
         : (module Interface.Int_printer.Provider_interface.S with type t = a)))
      interface;
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Int_printer
      (cache_state interface)
  in
  let float_printer_lookup () =
    (fun (type a) (interface : (a, _) Provider.Interface.t) ->
      ignore
        (Provider.Interface.lookup
           interface
           ~class_id:Interface.Float_printer.Provider_interface.Float_printer
         : (module Interface.Float_printer.Provider_interface.S with type t = a)))
      interface;
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Float_printer
      (cache_state interface)
  in
  let int_printer_lookup_opt () =
    require
      [%here]
      (Option.is_some
         (Provider.Interface.lookup_opt
            interface
            ~class_id:Interface.Int_printer.Provider_interface.Int_printer));
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Int_printer
      (cache_state interface)
  in
  let float_printer_lookup_opt () =
    require
      [%here]
      (Option.is_some
         (Provider.Interface.lookup_opt
            interface
            ~class_id:Interface.Float_printer.Provider_interface.Float_printer));
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Float_printer
      (cache_state interface)
  in
  let int_printer_implements () =
    let pre_cache_state = cache_state interface in
    require
      [%here]
      (Provider.Interface.implements
         interface
         ~class_id:Interface.Int_printer.Provider_interface.Int_printer);
    let post_cache_state = cache_state interface in
    require_equal [%here] (module Cache_state) pre_cache_state post_cache_state
  in
  let float_printer_implements () =
    let pre_cache_state = cache_state interface in
    require
      [%here]
      (Provider.Interface.implements
         interface
         ~class_id:Interface.Float_printer.Provider_interface.Float_printer);
    let post_cache_state = cache_state interface in
    require_equal [%here] (module Cache_state) pre_cache_state post_cache_state
  in
  (* At first, the cache is initialized with a brittle value. We don't register
     it. Let's start with a lookup. *)
  int_printer_lookup ();
  show_printer_cache ();
  [%expect {| Int_printer |}];
  (* Then we show that the [lookup] function triggers cache transitions. *)
  int_printer_lookup ();
  show_printer_cache ();
  [%expect {| Int_printer |}];
  float_printer_lookup ();
  show_printer_cache ();
  [%expect {| Float_printer |}];
  int_printer_lookup ();
  show_printer_cache ();
  [%expect {| Int_printer |}];
  (* Same with the [lookup_opt] function. *)
  float_printer_lookup_opt ();
  show_printer_cache ();
  [%expect {| Float_printer |}];
  int_printer_lookup_opt ();
  show_printer_cache ();
  [%expect {| Int_printer |}];
  int_printer_lookup ();
  show_printer_cache ();
  [%expect {| Int_printer |}];
  (* Finally, we show that using [implements] doesn't affect the cache. *)
  int_printer_implements ();
  float_printer_implements ();
  int_printer_implements ();
  float_printer_lookup_opt ();
  show_printer_cache ();
  [%expect {| Float_printer |}];
  int_printer_implements ();
  float_printer_implements ();
  int_printer_implements ();
  [%expect {||}];
  ()
;;

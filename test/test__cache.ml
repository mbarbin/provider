(* The trait lookup implementation uses a cache to speed up the search when
   looking up the same trait over and over in a row. In this test we monitor how
   the cache is updated. *)

module Cache_state = struct
  [@@@coverage off]

  type t =
    | None
    | Int_printer
    | Float_printer
  [@@deriving equal, sexp_of]
end

let%expect_test "override" =
  let num_printer = Test_providers.Num_printer.make () in
  let cache_state_of_uid uid =
    if
      Provider.Trait.Uid.equal
        uid
        (Test_interfaces.Int_printer.Provider_interface.int_printer |> Provider.Trait.uid)
    then Cache_state.Int_printer
    else if
      Provider.Trait.Uid.equal
        uid
        (Test_interfaces.Float_printer.Provider_interface.float_printer
         |> Provider.Trait.uid)
    then Cache_state.Float_printer
    else assert false [@coverage off]
  in
  let cache_state provider =
    match Provider.Private.cache provider with
    | None -> Cache_state.None
    | Some uid -> cache_state_of_uid uid
  in
  let show_cache provider = print_s [%sexp (cache_state provider : Cache_state.t)] in
  let show_printer_cache () =
    let (Provider.T { t = _; provider }) = num_printer in
    show_cache provider
  in
  (* An empty provider has no cache. *)
  show_cache (Provider.make []);
  [%expect {| None |}];
  let (Provider.T { t = _; provider }) = num_printer in
  let int_printer_lookup () =
    (fun (type a) (provider : (a, _) Provider.t) ->
       ignore
         (Provider.lookup
            provider
            ~trait:Test_interfaces.Int_printer.Provider_interface.int_printer
          : (module Test_interfaces.Int_printer.Provider_interface.S with type t = a)))
      provider;
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Int_printer
      (cache_state provider)
  in
  let float_printer_lookup () =
    (fun (type a) (provider : (a, _) Provider.t) ->
       ignore
         (Provider.lookup
            provider
            ~trait:Test_interfaces.Float_printer.Provider_interface.float_printer
          : (module Test_interfaces.Float_printer.Provider_interface.S with type t = a)))
      provider;
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Float_printer
      (cache_state provider)
  in
  let int_printer_lookup_opt () =
    require
      [%here]
      (Option.is_some
         (Provider.lookup_opt
            provider
            ~trait:Test_interfaces.Int_printer.Provider_interface.int_printer));
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Int_printer
      (cache_state provider)
  in
  let float_printer_lookup_opt () =
    require
      [%here]
      (Option.is_some
         (Provider.lookup_opt
            provider
            ~trait:Test_interfaces.Float_printer.Provider_interface.float_printer));
    require_equal
      [%here]
      (module Cache_state)
      Cache_state.Float_printer
      (cache_state provider)
  in
  let int_printer_implements () =
    let pre_cache_state = cache_state provider in
    require
      [%here]
      (Provider.implements
         provider
         ~trait:Test_interfaces.Int_printer.Provider_interface.int_printer);
    let post_cache_state = cache_state provider in
    require_equal [%here] (module Cache_state) pre_cache_state post_cache_state
  in
  let float_printer_implements () =
    let pre_cache_state = cache_state provider in
    require
      [%here]
      (Provider.implements
         provider
         ~trait:Test_interfaces.Float_printer.Provider_interface.float_printer);
    let post_cache_state = cache_state provider in
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

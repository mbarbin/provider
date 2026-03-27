# Lookup Strategy

In this part of the documentation, we will set aside typing considerations
and focus on the strategy used to search for a trait at runtime within a
provider construct.

We will describe the currently implemented strategy and discuss possible
trade-offs.

## Runtime representation

Under the hood, a provider is an array where each cell is a binding. This
binding is a pair that associates a trait with its implementation.

To simplify, we will consider traits as integers and implementations as
strings. This makes the examples easier to read.

```ocaml
type impl = string
type trait = int
type provider = (trait * impl) array
```

We are interested in defining lookup strategies — functions that return the
implementation bound to a trait if the provider implements it.

```ocaml
type lookup_strategy = provider -> trait -> impl option
```

## Lookup Strategies

### Linear Scan

One possible lookup strategy is a linear scan of the array. For example:

```ocaml
let linear_scan : lookup_strategy =
  fun provider trait ->
  Array.find_map provider ~f:(fun (t, impl) -> Option.some_if (Int.equal t trait) impl)
;;

let print_dyn dyn = Format.printf "%a@." Pp.to_fmt (Dyn.pp dyn)

let test scan provider trait =
  print_dyn (scan provider trait |> Dyn.option Dyn.string);
  ()
;;

let%expect_test "linear_scan" =
  test linear_scan [| 134, "Hello"; 7, "World" |] 13;
  [%expect {| None |}];
  test linear_scan [| 134, "Hello"; 7, "World" |] 134;
  [%expect {| Some "Hello" |}];
  ()
;;
```

This strategy is simple and does not require any specific ordering within the
bindings. It runs in `O(n)`, where `n` is the number of traits. When `n` is
small, this is likely to be very fast, if not the fastest implementation.

### Binary Search

This is a slight twist on the linear scan.

```ocaml
let binary_search : lookup_strategy =
  fun provider trait ->
  let rec loop lo hi =
    if lo >= hi
    then None
    else (
      let mid = lo + ((hi - lo) / 2) in
      let key, _ = provider.(mid) in
      match Int.compare key trait with
      | Eq -> Some (snd provider.(mid))
      | Lt -> loop (mid + 1) hi
      | Gt -> loop lo mid)
  in
  loop 0 (Array.length provider)
;;
```

In this strategy, the bindings need to be ordered in increasing order
according to their traits. Typically, a provider is constructed once and then
passed around and used many times during its lifetime. This approach lends
itself well to the idea of performing a bit more computation upfront to
achieve a systematic speedup during subsequent use.

```ocaml
let make_provider bindings =
  bindings |> List.sort ~compare:(fun (i, _) (j, _) -> Int.compare i j) |> Array.of_list
;;

let provider = make_provider [ 99, "Hello"; 7, "World"; 134, "Foo"; 17, "Bar" ]

let provider_to_dyn a =
  Dyn.array (fun (trait, impl) -> Dyn.Tuple [ Dyn.int trait; Dyn.string impl ]) a
;;

let%expect_test "sorted provider" =
  print_dyn (provider |> provider_to_dyn);
  [%expect {| [| (7, "World");  (17, "Bar");  (99, "Hello");  (134, "Foo") |] |}];
  ()
;;

let%expect_test "binary_search" =
  test binary_search provider 13;
  [%expect {| None |}];
  test binary_search provider 99;
  [%expect {| Some "Hello" |}];
  ()
;;
```

### Cache

If we are frequently searching for the same trait multiple times in a row, we
might benefit from using a cache.

When we started implementing the library, we were unsure whether a cache
would be useful. We decided to implement a simple caching mechanism as a
proof of concept. This approach allowed us to evaluate whether we needed to
change the representation early on, avoiding potential incompatible changes
in later versions.

The strategy we implemented uses the first cell (index 0) of the array to
store the most recently accessed trait. The rest of the array (from index 1
to n) is kept sorted by trait and is searched using a binary search.

```ocaml
let binary_search_with_cache : lookup_strategy =
  fun provider trait ->
  if Array.length provider = 0
  then None [@coverage off]
  else (
    let cache = provider.(0) in
    if Int.equal (fst cache) trait
    then (
      print_endline "Hitting the cache!";
      Some (snd cache))
    else (
      let rec loop lo hi =
        if lo >= hi
        then None
        else (
          let mid = lo + ((hi - lo) / 2) in
          let key, _ = provider.(mid) in
          match Int.compare key trait with
          | Eq -> Some mid
          | Lt -> loop (mid + 1) hi
          | Gt -> loop lo mid)
      in
      match loop 1 (Array.length provider) with
      | None -> None
      | Some i ->
        let impl = snd provider.(i) in
        provider.(0) <- trait, impl;
        Some impl))
;;

let make_provider_with_cache bindings =
  match bindings |> List.sort ~compare:(fun (i, _) (j, _) -> Int.compare i j) with
  | [] -> [||] [@coverage off]
  | hd :: tl ->
    (* Initialize the cache arbitrarily with the smallest trait. *)
    Array.of_list (hd :: hd :: tl)
;;

let provider = make_provider_with_cache [ 99, "Hello"; 7, "World"; 134, "Foo"; 17, "Bar" ]

let%expect_test "binary_search_with_cache" =
  print_dyn (provider |> provider_to_dyn);
  [%expect
    {|
    [| (7, "World")
    ;  (7, "World")
    ;  (17, "Bar")
    ;  (99, "Hello")
    ;  (134, "Foo")
    |]
    |}];
  test binary_search_with_cache provider 13;
  [%expect {| None |}];
  test binary_search_with_cache provider 99;
  [%expect {| Some "Hello" |}];
  print_dyn (provider |> provider_to_dyn);
  [%expect
    {|
    [| (99, "Hello")
    ;  (7, "World")
    ;  (17, "Bar")
    ;  (99, "Hello")
    ;  (134, "Foo")
    |]
    |}];
  test binary_search_with_cache provider 99;
  [%expect
    {|
    Hitting the cache!
    Some "Hello"
    |}];
  ()
;;
```

## Future Plans

We are not entirely convinced that the cache is useful. It is difficult to
determine its utility without more information about how the library is
actually used. Additionally, we envision that:

- Defaulting to a linear scan may be faster overall in cases where the number
of traits is small.
- The cache introduces complexity if the provider is accessed by multiple
domains in parallel. In such cases, you may prefer to disable it entirely.

Therefore, our future plans may include tweaking the lookup strategy,
conducting benchmarks, and providing more control to the user.

We are not concerned about breaking changes because:

1. Building a provider is done through a function call to which we could add
optional parameters, such as `~enable_cache:bool`.

2. There exists a simple criterion to determine whether a provider has a
cache location at position 0:

```ocaml
let uses_a_cache provider =
  Array.length provider >= 2 && fst provider.(0) >= fst provider.(1)
;;

let bindings = [ 99, "Hello"; 7, "World"; 134, "Foo"; 17, "Bar" ]

let%expect_test "uses_a_cache" =
  print_dyn (uses_a_cache (make_provider bindings) |> Dyn.bool);
  [%expect {| false |}];
  print_dyn (uses_a_cache (make_provider_with_cache bindings) |> Dyn.bool);
  [%expect {| true |}]
;;
```

This demonstrates how we can recognize whether caching was enabled at
provider creation time and act accordingly.

## Conclusion

In this documentation, we've explained the runtime representation of a
provider and discussed different lookup strategies for searching traits.
We've documented the current strategy, which uses a binary search and a
simple cache, and concluded by offering some ideas for future improvements
while maintaining good compatibility with the existing code.

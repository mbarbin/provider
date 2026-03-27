# Test magic 2

This is a variation of `./test__magic.md` without type ids.

At its heart, it was based on the fact that you could add arguments to the
trait constructors. Now, it is no longer possible by design, so the issues
associated with this test are no longer applicable.

```ocaml
module type S = sig
  type t

  val t : t
end

type (_, _, _) Provider.Trait.t +=
  | A :
      'something Type.Id.t
      -> (_, (module S with type t = 'something), [> `A ]) Provider.Trait.t
;;
```

```ansi
[1mLines 7-10, characters 0-75[0m:
 7 | type (_, _, _) Provider.Trait.t +=
 8 |   | A :
 9 |       'something Type.Id.t
10 |       -> (_, (module S with type t = 'something), [> `A ]) Provider.Trait.t
[1;31mError[0m: Type definition [1mProvider.Trait.t[0m is not extensible
```

Now that we have to go through the `Trait.Create` functors, there just isn't
any way to build a trait with this extra argument.

## For reference

We're keeping the rest of the test for reference only, it cannot be written
with recent versions of the library anymore.

```ocaml

module type S = sig
  type t

  val t : t
end

type (_, _, _) Provider.Trait.t +=
  | A : 'a -> (_, (module S with type t = 'a), [> `A ]) Provider.Trait.t

let () = Provider.Trait.Info.register_name (A ()) ~name:"A"

let impl (type a) arg ~check_trait =
  Provider.Private.Trait.implement_unsafe
    (A arg)
    ~impl:
      (module struct
        type t = a

        let t = arg
      end)
    ~check_trait
;;

let%expect_test "magic" =
  let make_provider ~check_trait =
    Provider.make
      [ (if true then impl 1 ~check_trait else impl "" ~check_trait [@coverage off]) ]
  in
  require_does_raise [%here] (fun () -> make_provider ~check_trait:true);
  [%expect
    {|
    ("Invalid usage of [Provider.Trait]: trait is not a valid extensible variant for this library"
     ((
       trait (
         (id   #id)
         (name A)))))
    |}];
  let provider = make_provider ~check_trait:false in
  require_does_raise [%here] (fun () ->
    (let module M = (val Provider.lookup provider ~trait:(A "0")) in
    print_string M.t) [@coverage off]);
  [%expect
    {|
    ("Invalid usage of [Provider.Trait]: Extensible variants with the same id are expected to be physically equal through the use of this library"
     ((
       trait (
         (id   #id)
         (name A)))))
    |}];
  ()
;;
```

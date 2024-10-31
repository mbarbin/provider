# Test magic 2

This is a variation of `./test__magic.md` without type ids.

At its heart, it was based on the fact that you could add arguments to the trait constructors. Now, it is no longer possible by design, so the issues associated with this test are no longer applicable.

```ocaml
module type S = sig
  type t

  val t : t
end

type (_, _, _) Provider.Trait.t +=
  | A :
      'something Base.Type_equal.Id.t
      -> (_, (module S with type t = 'something), [> `A ]) Provider.Trait.t
```

## For reference

We're keeping the rest of the test for reference only, it cannot be written with recent versions of the library anymore.

<!-- $MDX skip -->
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
  let make_handler ~check_trait =
    Provider.Handler.make
      [ (if true then impl 1 ~check_trait else impl "" ~check_trait [@coverage off]) ]
  in
  require_does_raise [%here] (fun () -> make_handler ~check_trait:true);
  [%expect
    {|
    ("Invalid usage of [Provider.Trait]: trait is not a valid extensible variant for this library"
     ((
       trait (
         (id   #id)
         (name A)))))
    |}];
  let handler = make_handler ~check_trait:false in
  require_does_raise [%here] (fun () ->
    (let module M = (val Provider.Handler.lookup handler ~trait:(A "0")) in
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

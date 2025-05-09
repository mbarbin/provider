# Test magic

This test monitors an example that caused an earlier version of the library to segfault. We keep it as regression test.

This test was originally contributed by @v-gb.

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
```mdx-error
Lines 7-10, characters 3-78:
Error: Type definition Provider.Trait.t is not extensible
```

Now that we have to go through the `Trait.Create` functors, there just isn't any way to build a trait with this extra argument.

## For reference

We're keeping the rest of the test for reference only, it cannot be written with recent versions of the library anymore.

<!-- $MDX skip -->
```ocaml

let id_int = Type_equal.Id.create ~name:"int" [%sexp_of: int]
let id_string = Type_equal.Id.create ~name:"string" [%sexp_of: string]
let () = Provider.Trait.Info.register_name (A id_int) ~name:"A"

let impl (type a) id value ~check_trait =
  Provider.Private.Trait.implement_unsafe
    (A id)
    ~impl:
      (module struct
        type t = a

        let t = value
      end)
    ~check_trait
;;

let%expect_test "magic" =
  let make_provider ~check_trait =
    Provider.make
      [ (if true
         then impl id_int 1 ~check_trait
         else impl id_string "" ~check_trait [@coverage off])
      ]
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
    (let module M = (val Provider.lookup provider ~trait:(A id_string)) in
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

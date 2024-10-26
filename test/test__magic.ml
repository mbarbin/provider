(* This test monitors an example that caused an earlier version of the library
   to segfault. We keep it as regression test.

   This test was contributed by @v-gb. *)

module type S = sig
  type t

  val t : t
end

type (_, _, _) Provider.Trait.t +=
  | A :
      'something Base.Type_equal.Id.t
      -> (_, (module S with type t = 'something), [> `A ]) Provider.Trait.t

let id_int = Type_equal.Id.create ~name:"int" [%sexp_of: int]
let id_string = Type_equal.Id.create ~name:"string" [%sexp_of: string]

let impl (type a) id value =
  Provider.Trait.implement
    (A id)
    ~impl:
      (module struct
        type t = a

        let t = value
      end)
;;

let%expect_test "magic" =
  let handler =
    Provider.Handler.make
      [ (if true then impl id_int 1 else impl id_string "" [@coverage off]) ]
  in
  require_does_raise [%here] (fun () ->
    (let module M = (val Provider.Handler.lookup handler ~trait:(A id_string)) in
    print_string M.t) [@coverage off]);
  [%expect
    {|
    ("Invalid usage of [Provider.Trait]: Extensible variants with the same id are expected to be physically equal through the use of this library"
     ((
       trait (
         (id   #id)
         (name Provider_test.Test__magic.A)))))
    |}];
  ()
;;

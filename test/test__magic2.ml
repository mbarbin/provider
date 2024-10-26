(* This is a variation of [test__magic.ml] without type ids. *)

module type S = sig
  type t

  val t : t
end

type (_, _, _) Provider.Trait.t +=
  | A : 'a -> (_, (module S with type t = 'a), [> `A ]) Provider.Trait.t

let impl (type a) arg =
  Provider.Trait.implement
    (A arg)
    ~impl:
      (module struct
        type t = a

        let t = arg
      end)
;;

let%expect_test "magic" =
  let handler =
    Provider.Handler.make [ (if true then impl 1 else impl "" [@coverage off]) ]
  in
  require_does_raise [%here] (fun () ->
    (let module M = (val Provider.Handler.lookup handler ~trait:(A "0")) in
    print_string M.t) [@coverage off]);
  [%expect
    {|
    ("Invalid usage of [Provider.Trait]: Extensible variants with the same id are expected to be physically equal through the use of this library"
     ((
       trait (
         (id   #id)
         (name Provider_test.Test__magic2.A)))))
    |}];
  ()
;;

(* In this test we check that the trait system allows to be used with module
   interfaces that are parametrized.

   There is also a tutorial for this, but we like to have both. Motivations are:

   - here we get better error messages and development experience when working
     on this (the tutorial is an mdx file, which has a slightly less advanced
     editor integration).

   - this test is checked by [more-ci] whereas the tutorial is not. *)

module type Mappable = sig
  type 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  type higher_kinded

  val inject : 'a t -> ('a -> higher_kinded) Higher_kinded.t
  val project : ('a -> higher_kinded) Higher_kinded.t -> 'a t
end

type mappable = [ `Mappable ]

module Mappable : sig
  val t
    : ( ('a -> 'higher_kinded) Higher_kinded.t
        , (module Mappable with type higher_kinded = 'higher_kinded)
        , [> mappable ] )
        Provider.Trait.t
end = Provider.Trait.Create1 (struct
    type (!'higher_kinded, 'a) t = ('a -> 'higher_kinded) Higher_kinded.t

    type 'higher_kinded module_type =
      (module Mappable with type higher_kinded = 'higher_kinded)
  end)

let map_n_times
  : type a t.
    ((a -> t) Higher_kinded.t, [> mappable ]) Provider.Handler.t
    -> (a -> t) Higher_kinded.t
    -> int
    -> f:(a -> a)
    -> (a -> t) Higher_kinded.t
  =
  fun handler t n ~f ->
  let module M = (val Provider.Handler.lookup handler ~trait:Mappable.t) in
  let at = M.project t in
  let rec loop n at = if n = 0 then at else loop (n - 1) (M.map at ~f) in
  M.inject (loop n at)
;;

module Higher_kinded_list = struct
  include List
  include Higher_kinded.Make (List)
end

module Higher_kinded_array = struct
  include Array
  include Higher_kinded.Make (Array)
end

module _ : Mappable with type 'a t = 'a list = Higher_kinded_list
module _ : Mappable with type 'a t = 'a array = Higher_kinded_array

let mappable_list ()
  : ( ('a -> Higher_kinded_list.higher_kinded) Higher_kinded.t
      , [> mappable ] )
      Provider.Handler.t
  =
  Provider.Handler.make
    [ Provider.Trait.implement Mappable.t ~impl:(module Higher_kinded_list) ]
;;

let mappable_array ()
  : ( ('a -> Higher_kinded_array.higher_kinded) Higher_kinded.t
      , [> mappable ] )
      Provider.Handler.t
  =
  Provider.Handler.make
    [ Provider.Trait.implement Mappable.t ~impl:(module Higher_kinded_array) ]
;;

let%expect_test "map_n_times" =
  let r =
    map_n_times
      (mappable_list ())
      (List.init 10 ~f:Fn.id |> Higher_kinded_list.inject)
      3
      ~f:(fun x -> x + 1)
    |> Higher_kinded_list.project
  in
  print_s [%sexp (r : int list)];
  [%expect {| (3 4 5 6 7 8 9 10 11 12) |}];
  let r =
    map_n_times
      (mappable_array ())
      ([| "a"; "b" |] |> Higher_kinded_array.inject)
      4
      ~f:(fun x -> x ^ x)
    |> Higher_kinded_array.project
  in
  print_s [%sexp (r : string array)];
  [%expect {| (aaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbb) |}];
  ()
;;

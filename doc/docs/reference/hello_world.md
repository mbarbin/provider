# Hello World

Below is a minimalist program that uses the Provider library (cheat sheet).

```ocaml
module type S = sig
  type t

  val show : t -> string
end

type show = [ `Show ]

module Show : sig
  val t : ('t, (module S with type t = 't), [> show ]) Provider.Trait.t
end = Provider.Trait.Create (struct
  type 'a module_type = (module S with type t = 'a)
end)

let print (Provider.T { t; provider }) =
  let module M = (val Provider.lookup provider ~trait:Show.t) in
  print_endline (M.show t)

let string_provider t =
  let provider =
    Provider.make
      [ Provider.implement Show.t
          ~impl:(module struct type t = string let show = String.uppercase_ascii end)
      ]
  in
  Provider.T { t; provider }
```

```ocaml
# print (string_provider "Hello World")
HELLO WORLD
- : unit = ()
```

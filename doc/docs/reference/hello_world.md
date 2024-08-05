# Hello World

Below is a minimalist program that uses the Provider library (cheat sheet).

```ocaml
module type S = sig
  type t

  val show : t -> string
end

type show = [ `Show ]

type (_, _, _) Provider.Trait.t +=
  Show : ('t, (module S with type t = 't), [> show ]) Provider.Trait.t

let print (Provider.T { t; interface }) =
  let module M = (val Provider.Interface.lookup interface ~trait:Show) in
  print_endline (M.show t)

let string_provider t =
  let interface =
    Provider.Interface.make
      [ Provider.Trait.implement Show
          ~impl:(module struct type t = string let show = Fun.id end)
      ]
  in
  Provider.T { t; interface }
```

```ocaml
# print (string_provider "Hello World")
Hello World
- : unit = ()
```

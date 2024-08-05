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

let print (Provider.T { t; handler }) =
  let module M = (val Provider.Handler.lookup handler ~trait:Show) in
  print_endline (M.show t)

let string_provider t =
  let handler =
    Provider.Handler.make
      [ Provider.Trait.implement Show
          ~impl:(module struct type t = string let show = Fun.id end)
      ]
  in
  Provider.T { t; handler }
```

```ocaml
# print (string_provider "Hello World")
Hello World
- : unit = ()
```

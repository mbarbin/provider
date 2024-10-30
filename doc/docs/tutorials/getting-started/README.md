# Getting Started

In this tutorial, we'll create a small module that can find all files of a given extension in a directory and show the number of lines for each file. This module will be implemented in OCaml and will demonstrate the use of the Provider library.

We'll create a functionality equivalent to the following bash script:

```sh
$ for file in $(ls -1 *.txt | sort) ; do wc -l $file; done
5 hello.txt
1 prelude.txt
```

## Library Requirements

The library will be parametrized by the ability to:
- List the entries from a directory.
- Load the contents of a file from disk.

We'll instantiate this library with an implementation based on OCaml Stdlib.

## Using a Functor

We'll start in familiar territory by making a first attempt using a functor. A functor in OCaml is a module that is parametrized by another module. This allows us to create flexible and reusable code.

### Trait

We'll use the term "Trait" to refer to the functionality we depend on in the parametrization. This is essentially a module signature that operates on a given type. The terminology is inspired by Rust. Here is our `READER` Trait:

```ocaml
module type READER = sig

  (** A type to hold some environment, could be [unit] if you are
      using [Unix], [Eio.Stdenv.t], etc. *)
  type t

  (** List the entries present in the directory at a given path. *)
  val readdir : t -> path:string -> string list

  (** Returns the contents of a file at a given path. *)
  val load_file : t -> path:string -> string
end
```

### Parametrized Library

With the `READER` Trait defined, we can now implement `Show_files`. Since we are defining `Show_files` as a functor, we can write logic that depends on the abilities provided by the `READER` Trait, even though we do not yet have access to an actual implementation for that Trait.

```ocaml
module Show_files (Reader : READER) : sig

  val print_files_with_ext : Reader.t -> path:string -> ext:string -> unit

end = struct

  let print_files_with_ext reader ~path ~ext =
    let entries = Reader.readdir reader ~path |> List.sort String.compare in
    let files = List.filter (String.ends_with ~suffix:ext) entries in
    files |> List.iter (fun file ->
      let contents = Reader.load_file reader ~path:(Filename.concat path file) in
      let line_count =
        List.length (String.split_on_char '\n' contents)
        - (if String.ends_with ~suffix:"\n" contents then 1 else 0)
      in
      Printf.printf "%d %s\n" line_count file)

end
```

### Provider

A provider supplies implementations for a set of Traits. Let's create an implementation for the `READER` Trait based on OCaml Stdlib.

```ocaml
module Sys_reader : READER with type t = unit = struct

  (* Sys doesn't need any internal environment. *)
  type t = unit

  let readdir () ~path = Sys.readdir path |> Array.to_list

  let load_file () ~path = In_channel.with_open_bin path In_channel.input_all
end
```

### Runtime Instantiation

Now it is time to instantiate our library, assuming we are in some client code that will decide on which provider to supply to our parametrized library:

```ocaml
module My_show_files = Show_files (Sys_reader)
```

And then use it:

```ocaml
# My_show_files.print_files_with_ext () ~path:"." ~ext:".txt"
5 hello.txt
1 prelude.txt
- : unit = ()
```

So far, we've done nothing with the Provider library. Please hang on, that's what the next section is about!

## Using Provider

### Installation

Provider is available through opam:

<!-- $MDX skip -->
```sh
$ opam install provider
```

Then, make sure to add `provider` (the name of the library) to your dune file (and deps in `dune-project`).

If you are not using opam or dune, we'll assume you're an expert and know what to do!

### Trait

To use Provider, first we have to create a new tag and a new type constructor that will be attached to our `READER` Trait. To do this, we:

- Create a tag type with a polymorphic variant that will be dedicated to our Trait.
- Add dynamically a new constructor to the `Provider.Trait.t` extensible variant. This uses an OCaml Language Extension named [Extensible variant types](https://ocaml.org/manual/5.2/extensiblevariants.html). This one has the particularity that it is also a [GADT](https://ocaml.org/manual/5.2/gadts.html#start-section)!

```ocaml
type reader = [ `Reader ]

module Reader : sig
  val t : ('t, (module READER with type t = 't), [> reader ]) Provider.Trait.t
end = struct
  type (_, _, _) Provider.Trait.t +=
    Reader : ('t, (module READER with type t = 't), [> reader ]) Provider.Trait.t

  let t = Reader
end
```

### Parametrized Library

Now that we're switching to using Provider, our module is no longer a functor. Rather, each of the functions that need provider functionality will take it as an extra parameter. The type `[> reader ] Provider.t` indicates that the provider required needs to implement *at least* the `reader` Trait, but it is allowed to implement other Traits too (the other bindings will be ignored).

```ocaml
module Show_files2 : sig

  val print_files_with_ext : [> reader ] Provider.t -> path:string -> ext:string -> unit

end = struct

  let print_files_with_ext (Provider.T { t = reader; handler }) ~path ~ext =
    let module R = (val Provider.Handler.lookup handler ~trait:Reader.t) in
    let entries = R.readdir reader ~path |> List.sort String.compare in
    let files = List.filter (String.ends_with ~suffix:ext) entries in
    files |> List.iter (fun file ->
      let contents = R.load_file reader ~path:(Filename.concat path file) in
      let line_count =
        List.length (String.split_on_char '\n' contents)
        - (if String.ends_with ~suffix:"\n" contents then 1 else 0)
      in
      Printf.printf "%d %s\n" line_count file)

end
```

Notice how we've slightly changed the beginning of the implementation of `print_files_with_ext`. This time around, we are finding the module `Reader` by doing an handler lookup, based on the Trait we are interested in.

The rest of the implementation hasn't actually changed one bit compared to our first functor example. You can get further convinced by this last sentence, considering the following tweak:

```ocaml
module Show_files3 : sig

  val print_files_with_ext : [> reader ] Provider.t -> path:string -> ext:string -> unit

end = struct

  let print_files_with_ext (Provider.T { t = reader; handler }) ~path ~ext =
    let module R = (val Provider.Handler.lookup handler ~trait:Reader.t) in
    let module M = Show_files (R) in
    M.print_files_with_ext reader ~path ~ext

end
```

This is a sort of hybrid of the two versions! In a real-world scenario, you would probably not carry both versions around, so this is just for the sake of the example (although, perhaps in certain cases, it can make sense to have both styles around. You'll decide on a case-by-case basis).

### Provider

In this section, we are showing what implementing a Trait looks like. This part is simplified, given that we already have implemented a version of our `Reader` Trait when we wrote `Sys_reader`. We're going to be able to re-use it here, and we are showing below really only the provider-specific bits:

```ocaml
let sys_reader () : [ `Reader ] Provider.t =
  Provider.T
    { t = ()
    ; handler =
        Provider.Handler.make
          [ Provider.Trait.implement Reader.t ~impl:(module Sys_reader) ]
    }
```

### Runtime Instantiation

Same as earlier, assuming we're now in client code, it is time to commit to a runtime implementation and instantiate a provider!

```ocaml
let my_sys_reader = sys_reader ()
```

We can then move on to enjoying the functionality offered by the parametrized library.

```ocaml
# Show_files2.print_files_with_ext my_sys_reader ~path:"." ~ext:".txt"
5 hello.txt
1 prelude.txt
- : unit = ()

# Show_files3.print_files_with_ext my_sys_reader ~path:"." ~ext:".txt"
5 hello.txt
1 prelude.txt
- : unit = ()
```

## Conclusion

In this tutorial, we've created a Trait, a library parametrized by it, a provider implementing that Trait, and finally some user code invoking the library with this provider, providing a complete tour of the functionality offered by the library.

More complex cases would involve providers implementing multiple Traits, parametrized libraries with functions expecting multiple Traits as well (with some arbitrary overlap). You'll also have the ability to conditionally depend on the availability of certain Traits implementation at runtime.

This granularity allows different providers to select which Traits to implement. They can even choose to cover only part of the functionality required by a parametrized library, leaving some functions aside. This provides a level of flexibility that is not achievable with a monolithic functor.

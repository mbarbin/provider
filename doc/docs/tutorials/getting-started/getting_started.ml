(*********************************************************************************)
(*  provider - Dynamic Dispatch with Traits                                      *)
(*  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: ISC                                                 *)
(*********************************************************************************)

(* @mdexp

   # Getting Started

   In this tutorial, we'll create a small module that can find all files of a
   given extension in a directory and show the number of lines for each file. This
   module will be implemented in OCaml and will demonstrate the use of the Provider
   library.

   We'll create a functionality equivalent to the following bash script: *)

let run_sh cmd =
  Printf.printf "```sh\n$ %s\n" cmd;
  flush stdout;
  let exit_code = Sys.command (Printf.sprintf "bash -c %s" (Filename.quote cmd)) in
  if exit_code <> 0 then Printf.printf "[%d]\n" exit_code [@coverage off];
  print_string "```"
;;

let%expect_test "bash example" =
  run_sh "for file in $(ls -1 *.txt | sort) ; do wc -l $file; done";
  (* @mdexp.snapshot *)
  [%expect
    {|
    ```sh
    $ for file in $(ls -1 *.txt | sort) ; do wc -l $file; done
    5 hello.txt
    2 world.txt
    ```
    |}]
;;

(* @mdexp

   ## Library Requirements

   The library will be parametrized by the ability to:
   - List the entries from a directory.
   - Load the contents of a file from disk.

   We'll instantiate this library with an implementation based on OCaml Stdlib.

   ## Using a Functor

   We'll start in familiar territory by making a first attempt using a functor. A
   functor in OCaml is a module that is parametrized by another module. This allows
   us to create flexible and reusable code.

   ### Trait

   We'll use the term "Trait" to refer to the functionality we depend on in the
   parametrization. This is essentially a module signature that operates on a given
   type. The terminology is inspired by Rust. Here is our `READER` Trait:

   @mdexp.code *)

module type READER = sig
  (** A type to hold some environment, could be [unit] if you are
      using [Unix], [Eio.Stdenv.t], etc. *)
  type t

  (** List the entries present in the directory at a given path. *)
  val readdir : t -> path:string -> string list

  (** Returns the contents of a file at a given path. *)
  val load_file : t -> path:string -> string
end

(* @mdexp

   ### Parametrized Library

   With the `READER` Trait defined, we can now implement `Show_files`. Since we are
   defining `Show_files` as a functor, we can write logic that depends on the
   abilities provided by the `READER` Trait, even though we do not yet have access
   to an actual implementation for that Trait.

   @mdexp.code *)

let line_count contents =
  List.length (String.split_on_char '\n' contents)
  - if String.ends_with ~suffix:"\n" contents then 1 else (0 [@coverage off])
;;

module Show_files (Reader : READER) : sig
  val print_files_with_ext : Reader.t -> path:string -> ext:string -> unit
end = struct
  let print_files_with_ext reader ~path ~ext =
    let entries = Reader.readdir reader ~path |> List.sort String.compare in
    let files = List.filter (String.ends_with ~suffix:ext) entries in
    files
    |> List.iter (fun file ->
      let contents = Reader.load_file reader ~path:(Filename.concat path file) in
      Printf.printf "%d %s\n" (line_count contents) file)
  ;;
end

(* @mdexp

   ### Provider

   A provider supplies implementations for a set of Traits. Let's create an
   implementation for the `READER` Trait based on OCaml Stdlib.

   @mdexp.code *)

module Sys_reader : READER with type t = unit = struct
  (* Sys doesn't need any internal environment. *)
  type t = unit

  let readdir () ~path = Sys.readdir path |> Array.to_list
  let load_file () ~path = In_channel.with_open_bin path In_channel.input_all
end

(* @mdexp

   ### Runtime Instantiation

   Now it is time to instantiate our library, assuming we are in some client code
   that will decide on which provider to supply to our parametrized library:

   @mdexp.code *)

module My_show_files = Show_files (Sys_reader)

(* @mdexp

   And then use it:

   @mdexp.code *)

let%expect_test "functor instantiation" =
  My_show_files.print_files_with_ext () ~path:"." ~ext:".txt";
  [%expect
    {|
    5 hello.txt
    2 world.txt
    |}];
  ()
;;

(* @mdexp

   So far, we've done nothing with the Provider library. Please hang on, that's
   what the next section is about!

   ## Using Provider

   ### Installation

   Provider is available through opam:

   ```sh
   $ opam install provider
   ```

   Then, make sure to add `provider` (the name of the library) to your dune file
   (and deps in `dune-project`).

   If you are not using opam or dune, we'll assume you're an expert and know what
   to do!

   ### Trait

   To use Provider, first we have to create a new tag and a new type constructor
   that will be attached to our `READER` Trait. To do this, we:

   - Create a tag type with a polymorphic variant that will be dedicated to our
   Trait.
   - Create a new trait with one of the `Provider.Trait.Create*` functors.

   @mdexp.code *)

type reader = [ `Reader ]

module Reader : sig
  val t : ('t, (module READER with type t = 't), [> reader ]) Provider.Trait.t
end = Provider.Trait.Create (struct
    type 't module_type = (module READER with type t = 't)
  end)

(* @mdexp

   ### Parametrized Library

   Now that we're switching to using Provider, our module is no longer a functor.
   Rather, each of the functions that need provider functionality will take it as an
   extra parameter. The type `[> reader ] Provider.packed` indicates that the
   provider required needs to implement *at least* the `reader` Trait, but it is
   allowed to implement other Traits too (the other bindings will be ignored).

   @mdexp.code *)

module Show_files2 : sig
  val print_files_with_ext
    :  [> reader ] Provider.packed
    -> path:string
    -> ext:string
    -> unit
end = struct
  let print_files_with_ext (Provider.T { t = reader; provider }) ~path ~ext =
    let module R = (val Provider.lookup provider ~trait:Reader.t) in
    let entries = R.readdir reader ~path |> List.sort String.compare in
    let files = List.filter (String.ends_with ~suffix:ext) entries in
    files
    |> List.iter (fun file ->
      let contents = R.load_file reader ~path:(Filename.concat path file) in
      Printf.printf "%d %s\n" (line_count contents) file)
  ;;
end

(* @mdexp

   Notice how we've slightly changed the beginning of the implementation of
   `print_files_with_ext`. This time around, we are finding the module `Reader` by
   doing a provider lookup, based on the Trait we are interested in.

   The rest of the implementation hasn't actually changed one bit compared to our
   first functor example. You can get further convinced by this last sentence,
   considering the following tweak:

   @mdexp.code *)

module Show_files3 : sig
  val print_files_with_ext
    :  [> reader ] Provider.packed
    -> path:string
    -> ext:string
    -> unit
end = struct
  let print_files_with_ext (Provider.T { t = reader; provider }) ~path ~ext =
    let module R = (val Provider.lookup provider ~trait:Reader.t) in
    let module M = Show_files (R) in
    M.print_files_with_ext reader ~path ~ext
  ;;
end

(* @mdexp

   This is a sort of hybrid of the two versions! In a real-world scenario, you
   would probably not carry both versions around, so this is just for the sake of
   the example (although, perhaps in certain cases, it can make sense to have both
   styles around. You'll decide on a case-by-case basis).

   ### Provider

   In this section, we are showing what implementing a Trait looks like. This part
   is simplified, given that we already have implemented a version of our `Reader`
   Trait when we wrote `Sys_reader`. We're going to be able to re-use it here, and
   we are showing below really only the provider-specific bits:

   @mdexp.code *)

let sys_reader () : [ `Reader ] Provider.packed =
  Provider.T
    { t = ()
    ; provider = Provider.make [ Provider.implement Reader.t ~impl:(module Sys_reader) ]
    }
;;

(* @mdexp

   ### Runtime Instantiation

   Same as earlier, assuming we're now in client code, it is time to commit to a
   runtime implementation and instantiate a provider!

   @mdexp.code *)

let my_sys_reader = sys_reader ()

(* @mdexp

   We can then move on to enjoying the functionality offered by the parametrized
   library.

   @mdexp.code *)

let%expect_test "provider instantiation" =
  Show_files2.print_files_with_ext my_sys_reader ~path:"." ~ext:".txt";
  [%expect
    {|
    5 hello.txt
    2 world.txt
    |}];
  Show_files3.print_files_with_ext my_sys_reader ~path:"." ~ext:".txt";
  [%expect
    {|
    5 hello.txt
    2 world.txt
    |}];
  ()
;;

(* @mdexp

   ## Conclusion

   In this tutorial, we've created a Trait, a library parametrized by it, a
   provider implementing that Trait, and finally some user code invoking the library
   with this provider, providing a complete tour of the functionality offered by the
   library.

   More complex cases would involve providers implementing multiple Traits,
   parametrized libraries with functions expecting multiple Traits as well (with
   some arbitrary overlap). You'll also have the ability to conditionally depend on
   the availability of certain Traits implementation at runtime.

   This granularity allows different providers to select which Traits to implement.
   They can even choose to cover only part of the functionality required by a
   parametrized library, leaving some functions aside. This provides a level of
   flexibility that is not achievable with a monolithic functor. *)

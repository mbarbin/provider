open! Import

type t = (int, string) Hashtbl.t

let create () = Hashtbl.create 16
let default = create ()
let set_name t trait ~name = Hashtbl.add t ~key:(Trait0.uid trait) ~data:name
let get_name t trait = Hashtbl.find_opt t (Trait0.uid trait)

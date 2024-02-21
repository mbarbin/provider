let phys_same t1 t2 = phys_equal (Stdlib.Obj.repr t1) (Stdlib.Obj.repr t2)

module Trait = struct
  type ('t, 'module_type, 'tag) t = ..

  module Info = struct
    type t = Stdlib.Obj.Extension_constructor.t

    let sexp_of_id_default _ = Sexp.Atom "#id"
    let sexp_of_id = ref sexp_of_id_default

    let sexp_of_t t =
      let sexp_of_id id = !sexp_of_id id in
      [%sexp
        { id = (Stdlib.Obj.Extension_constructor.id t : id)
        ; name = (Stdlib.Obj.Extension_constructor.name t : string)
        }]
    ;;
  end

  let info = Stdlib.Obj.Extension_constructor.of_val

  module Uid = Int

  let uid t =
    Stdlib.Obj.Extension_constructor.id (Stdlib.Obj.Extension_constructor.of_val t)
  ;;

  let compare_by_uid id1 id2 = Uid.compare (uid id1) (uid id2)
  let same (id1 : _ t) (id2 : _ t) = phys_same id1 id2

  module Implementation = struct
    type ('t, 'module_type, 'tag) trait = ('t, 'module_type, 'tag) t

    type _ t =
      | T :
          { trait : ('t, 'module_type, _) trait
          ; impl : 'module_type
          }
          -> 't t

    let uid (T { trait; impl = _ }) = uid trait
    let info (T { trait; impl = _ }) = info trait

    let compare_by_uid (T { trait = id1; _ }) (T { trait = id2; _ }) =
      compare_by_uid id1 id2
    ;;
  end

  let implement (type a i) (trait : (a, i, _) t) ~(impl : i) : a Implementation.t =
    Implementation.T { trait; impl }
  ;;
end

module Interface = struct
  (* We sort the element by their extension_id in increasing order. Element.(0)
     is a cache of the most recently looked up method. *)
  type ('t, -'tags) t = 't Trait.Implementation.t array

  let make (type a) (implementations : a Trait.Implementation.t list) : (a, _) t =
    let implementations =
      let table = Hashtbl.create (module Trait.Uid) in
      List.iter implementations ~f:(fun implementation ->
        Hashtbl.set table ~key:(Trait.uid implementation) ~data:implementation);
      Hashtbl.data table |> List.sort ~compare:Trait.Implementation.compare_by_uid
    in
    match implementations with
    | [] -> [||]
    | hd :: _ ->
      (* We initialize the cache arbitrarily with the left most trait. *)
      Array.of_list (hd :: implementations)
  ;;

  let same_trait_uids : type a tags1 tags2. (a, tags1) t -> (a, tags2) t -> bool =
    fun t1 t2 ->
    (* We skip the cell 0 which contains the cache. *)
    if Array.length t1 <> Array.length t2
    then false
    else
      Array.for_alli t1 ~f:(fun i implementation ->
        i = 0 || 0 = Trait.Implementation.compare_by_uid implementation t2.(i))
  ;;

  let is_empty t = Array.length t = 0
  let cache t = if Array.length t = 0 then None else Some (Trait.Implementation.uid t.(0))

  let implementations t =
    match Array.to_list t with
    | [] -> []
    | _ :: tl -> tl
  ;;

  let extend t ~with_ = make (implementations t @ with_)

  let rec binary_search
    : type a implementation tags b.
      (a, tags) t
      -> trait:(a, implementation, tags) Trait.t
      -> update_cache:bool
      -> if_not_found:(trait_info:Trait.Info.t -> b)
      -> if_found:(implementation -> b)
      -> from:int
      -> to_:int
      -> b
    =
    fun t ~trait ~update_cache ~if_not_found ~if_found ~from ~to_ ->
    if from > to_
    then if_not_found ~trait_info:(Trait.info trait)
    else (
      let mid = (from + to_) / 2 in
      let (Trait.Implementation.T { trait = elt; impl } as implementation) = t.(mid) in
      match Trait.compare_by_uid elt trait |> Ordering.of_int with
      | Equal ->
        if update_cache then t.(0) <- implementation;
        if_found (Stdlib.Obj.magic impl)
      | Less ->
        binary_search t ~trait ~update_cache ~if_not_found ~if_found ~from:(mid + 1) ~to_
      | Greater ->
        binary_search t ~trait ~update_cache ~if_not_found ~if_found ~from ~to_:(mid - 1))
  ;;

  let make_lookup
    : type a implementation tags b.
      (a, tags) t
      -> trait:(a, implementation, tags) Trait.t
      -> update_cache:bool
      -> if_not_found:(trait_info:Trait.Info.t -> b)
      -> if_found:(implementation -> b)
      -> b
    =
    fun t ~trait ~update_cache ~if_not_found ~if_found ->
    if Array.length t = 0
    then if_not_found ~trait_info:(Trait.info trait)
    else (
      let (Trait.Implementation.T { trait = cached_id; impl }) = t.(0) in
      if Trait.same trait cached_id
      then if_found (Stdlib.Obj.magic impl)
      else
        binary_search
          t
          ~trait
          ~update_cache
          ~if_not_found
          ~if_found
          ~from:1
          ~to_:(Array.length t - 1))
  ;;

  module If_not_found = struct
    let raise ~trait_info =
      raise_s [%sexp "Trait not implemented", { trait_info : Trait.Info.t }]
    ;;

    let none ~trait_info:_ = None
    let false_ ~trait_info:_ = false
  end

  let lookup t ~trait =
    make_lookup
      t
      ~trait
      ~update_cache:true
      ~if_not_found:If_not_found.raise
      ~if_found:Fn.id
  ;;

  let lookup_opt t ~trait =
    make_lookup
      t
      ~trait
      ~update_cache:true
      ~if_not_found:If_not_found.none
      ~if_found:Option.return
  ;;

  let implements t ~trait =
    (* Only checking for the availability of the trait doesn't affect the cache,
       we leave it untouched in this case. *)
    make_lookup
      t
      ~trait
      ~update_cache:false
      ~if_not_found:If_not_found.false_
      ~if_found:(Fn.const true)
  ;;
end

type -'tags t =
  | T :
      { t : 't
      ; interface : ('t, 'tags) Interface.t
      }
      -> 'tags t

module Private = struct
  module Interface = Interface
end

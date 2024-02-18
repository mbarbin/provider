let phys_same t1 t2 = phys_equal (Stdlib.Obj.repr t1) (Stdlib.Obj.repr t2)

module Class_id = struct
  type ('t, 'implementation, 'tag) t = ..

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
end

module Class = struct
  type _ t =
    | T :
        { class_id : ('t, 'implementation, _) Class_id.t
        ; implementation : 'implementation
        }
        -> 't t

  let uid (T { class_id; implementation = _ }) = Class_id.uid class_id
  let info (T { class_id; implementation = _ }) = Class_id.info class_id

  let compare_by_uid (T { class_id = id1; _ }) (T { class_id = id2; _ }) =
    Class_id.compare_by_uid id1 id2
  ;;

  let implement (type a i) ~(class_id : (a, i, _) Class_id.t) (implementation : i) : a t =
    T { class_id; implementation }
  ;;
end

module Interface = struct
  (* We sort the element by their extension_id in increasing order. Element.(0)
     is a cache of the most recently looked up method. *)
  type ('t, -'tags) t = 't Class.t array

  let make (type a) (classes : a Class.t list) : (a, _) t =
    let classes =
      let table = Hashtbl.create (module Class_id.Uid) in
      List.iter classes ~f:(fun class_ ->
        Hashtbl.set table ~key:(Class_id.uid class_) ~data:class_);
      Hashtbl.data table |> List.sort ~compare:Class.compare_by_uid
    in
    match classes with
    | [] -> [||]
    | hd :: tl -> Array.of_list (hd :: hd :: tl)
  ;;

  let same_class_uids : type a tags1 tags2. (a, tags1) t -> (a, tags2) t -> bool =
    fun t1 t2 ->
    (* We skip the cell 0 which contains the cache. *)
    if Array.length t1 <> Array.length t2
    then false
    else
      Array.for_alli t1 ~f:(fun i class_ ->
        i = 0 || 0 = Class.compare_by_uid class_ t2.(i))
  ;;

  let is_empty t = Array.length t = 0
  let cache t = if Array.length t = 0 then None else Some (Class.uid t.(0))

  let classes t =
    match Array.to_list t with
    | [] -> []
    | _ :: tl -> tl
  ;;

  let extend t ~with_ = make (classes t @ with_)

  let not_implemented ~class_info =
    raise_s [%sexp "Class not implemented", { class_info : Class_id.Info.t }]
  ;;

  let rec binary_search
    : type a implementation tags b.
      (a, tags) t
      -> class_id:(a, implementation, tags) Class_id.t
      -> update_cache:bool
      -> if_not_found:(class_info:Class_id.Info.t -> b)
      -> if_found:(implementation -> b)
      -> from:int
      -> to_:int
      -> b
    =
    fun t ~class_id ~update_cache ~if_not_found ~if_found ~from ~to_ ->
    if from > to_
    then if_not_found ~class_info:(Class_id.info class_id)
    else (
      let mid = (from + to_) / 2 in
      let (Class.T { class_id = elt; implementation }) = t.(mid) in
      match Class_id.compare_by_uid elt class_id |> Ordering.of_int with
      | Equal ->
        if update_cache then t.(0) <- t.(mid);
        if_found (Stdlib.Obj.magic implementation)
      | Less ->
        binary_search
          t
          ~class_id
          ~update_cache
          ~if_not_found
          ~if_found
          ~from:(mid + 1)
          ~to_
      | Greater ->
        binary_search
          t
          ~class_id
          ~update_cache
          ~if_not_found
          ~if_found
          ~from
          ~to_:(mid - 1))
  ;;

  let make_lookup
    : type a implementation tags b.
      (a, tags) t
      -> class_id:(a, implementation, tags) Class_id.t
      -> update_cache:bool
      -> if_not_found:(class_info:Class_id.Info.t -> b)
      -> if_found:(implementation -> b)
      -> b
    =
    fun t ~class_id ~update_cache ~if_not_found ~if_found ->
    if Array.length t = 0 then not_implemented ~class_info:(Class_id.info class_id);
    let (Class.T { class_id = cached_id; implementation }) = t.(0) in
    if Class_id.same class_id cached_id
    then if_found (Stdlib.Obj.magic implementation)
    else
      binary_search
        t
        ~class_id
        ~update_cache
        ~if_not_found
        ~if_found
        ~from:1
        ~to_:(Array.length t - 1)
  ;;

  let lookup t ~class_id =
    make_lookup
      t
      ~class_id
      ~update_cache:true
      ~if_not_found:not_implemented
      ~if_found:Fn.id
  ;;

  let lookup_opt t ~class_id =
    make_lookup
      t
      ~class_id
      ~update_cache:true
      ~if_not_found:(fun ~class_info:_ -> None)
      ~if_found:Option.return
  ;;

  let implements t ~class_id =
    (* Only checking for the availability of the class doesn't affect the cache,
       we leave it untouched in this case. *)
    make_lookup
      t
      ~class_id
      ~update_cache:false
      ~if_not_found:(fun ~class_info:_ -> false)
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

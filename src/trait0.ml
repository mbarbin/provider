type ('t, 'module_type) ext = ..

type ('t, 'module_type, 'tag) t =
  { ext : ('t, 'module_type) ext
  ; same_witness : 'm2. ('t, 'm2) ext -> ('module_type, 'm2) Type_eq_opt.t
  }

let uid (t : _ t) = Obj.Extension_constructor.id (Obj.Extension_constructor.of_val t.ext)

let same_witness : ('t, 'mt1, _) t -> ('t, 'mt2, _) t -> ('mt1, 'mt2) Type_eq_opt.t =
  fun t1 t2 -> t1.same_witness t2.ext
;;

module Create0 (X : sig
    type t
    type module_type
  end) =
struct
  type (_, _) ext += T : (X.t, X.module_type) ext

  let same_witness (type m2) t2 : (X.module_type, m2) Type_eq_opt.t =
    match (t2 : (X.t, m2) ext) with
    | T -> Type_eq_opt.Equal
    | _ -> Not_equal
  ;;

  let t = { ext = T; same_witness }
end

module Create1 (X : sig
    type !'a t
    type 'a module_type
  end) =
struct
  type (_, _) ext += T : ('a X.t, 'a X.module_type) ext

  let same_witness (type a m2) t2 : (a X.module_type, m2) Type_eq_opt.t =
    match (t2 : (a X.t, m2) ext) with
    | T -> Type_eq_opt.Equal
    | _ -> Not_equal
  ;;

  let t = { ext = T; same_witness }
end

module Create2 (X : sig
    type (!'a, !'b) t
    type ('a, 'b) module_type
  end) =
struct
  type (_, _) ext += T : (('a, 'b) X.t, ('a, 'b) X.module_type) ext

  let same_witness (type a b m2) t2 : ((a, b) X.module_type, m2) Type_eq_opt.t =
    match (t2 : ((a, b) X.t, m2) ext) with
    | T -> Type_eq_opt.Equal
    | _ -> Not_equal
  ;;

  let t = { ext = T; same_witness }
end

module Create (X : sig
    type 'a module_type
  end) =
Create1 (struct
    type !'a t = 'a
    type 'a module_type = 'a X.module_type
  end)

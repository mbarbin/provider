type _ t =
  | T :
      { trait : ('t, 'module_type, _) Trait0.t
      ; impl : 'module_type
      }
      -> 't t

let implement (type a i) (trait : (a, i, _) Trait0.t) ~(impl : i) : a t =
  T { trait; impl }
;;

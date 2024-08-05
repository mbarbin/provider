type _ t = private
  | T :
      { trait : ('t, 'module_type, _) Trait0.t
      ; implementation : 'module_type
      }
      -> 't t

val implement : ('t, 'module_type, _) Trait0.t -> impl:'module_type -> 't t

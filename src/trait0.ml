type ('t, 'module_type, 'tag) t = ..

let is_valid (t : _ t) =
  let extension_constructor = Obj.Extension_constructor.of_val t in
  Obj.repr t == Obj.repr extension_constructor
;;

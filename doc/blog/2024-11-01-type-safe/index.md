---
slug: type-safe
title: Announcing Provider 0.0.11
authors: [mbarbin]
tags: [api-changes]
---

We are excited to announce the release of Provider `0.0.11`, which brings improvements and changes to the library.

We hope you find these updates valuable. For more detailed information, please refer to the complete changelog. As always, we welcome your feedback and contributions. Happy coding!

<!-- truncate -->

Here are the key highlights:

## Redesign of Traits

In this release, we have redesigned the way traits are defined to remove the need for `Obj.magic` in the implementation. Traits are now created through applications of functors, allowing us to replace the unsafe casting function with a safer alternative. This work was done in collaboration with @v-gb.

**Previous construct**:

```ocaml
module My_trait : sig
  val t
    : ( 't
      , (module S with type t = 't)
      , [> `My_trait ]
      ) Provider.Trait.t
end = struct
  type (_, _, _) Provider.Trait.t +=
    My_trait
     : ( 't
       , (module S with type t = 't)
       , [> `My_trait ]
       ) Provider.Trait.t

  let t = My_trait
end
```

**New construct**:

```ocaml
module My_trait : sig
  val t
    : ( 't
      , (module S with type t = 't)
      , [> `My_trait ]
      ) Provider.Trait.t
end = Provider.Trait.Create (struct
  type 'a module_type = (module S with type t = 'a)
end)
```

## Expanded Platform Support

Provider is now tested on Windows and macOS, and is available with OCaml 4.14. Special thanks to @jonahbeckford for suggesting the build with 4.14 and contributing towards that goal.

## API Changes

We have made changes to the API and effectively removed the word 'handler' from the project terminology. This change simplifies the interface and makes it more ergonomic.

Most of the user facing renames can be summarized as follows:

**Functions**

- Provider.Trait.implement => Provider.implement
- Provider.Handler.make => Provider.make
- Provider.Handler.lookup => Provider.lookup

**Types**

- Provider.Handler.t => Provider.t
- Provider.t => Provider.packed

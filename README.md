# provider

⚠️ A word of caution from the author of `provider` (mbarbin):

> When I discovered a certain code pattern allowing dynamic dispatch in the Eio project I was intrigued and wanted to explore it further. I created this repo and used it as a dependency in prototypes of other projects (vcs, I say more about this below). I did release it to opam thinking I would need it to eventually release `vcs`, however after some time the design of vcs was [switched](https://github.com/mbarbin/vcs/pull/56) to one using OCaml Objects under the hood. Today I no longer have a use case for `Provider`. It took me some back and forth to get there, but now I suspect there is something perhaps fundamentally unsatisfying about this pattern, which I would attempt at summarizing as follows : it's trying to re-implement an object system using GADTs in a language that already has one (OCaml Objects). If you look past the syntax indeed, an object even dressed-up as a GADT is still an object - it is no longer so clear to me why you should't use an object-system when you need one.
>
> I think I'll probably try hinting towards deprecating that project from opam, and eventually advocate for archiving it. I advise against building new things with `provider` as a dependency.
>
> See also:
> - https://gr-im.github.io/a/dependency-injection.html

---

[![CI Status](https://github.com/mbarbin/provider/workflows/ci/badge.svg)](https://github.com/mbarbin/provider/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/provider/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/provider?branch=main)
[![Deploy Doc Status](https://github.com/mbarbin/provider/workflows/deploy-doc/badge.svg)](https://github.com/mbarbin/provider/actions/workflows/deploy-doc.yml)

## Introduction

Provider is an OCaml library for creating Traits and Interfaces. It allows you to define the functionality needed by a library without committing to a specific implementation.

In essence, Provider handles dynamic dispatch, where the target of a function call is not known until runtime. This is particularly useful in situations where there are multiple ways to provide a certain functionality, and the choice of provider is determined by the end user.

## Documentation

Published [here](https://mbarbin.github.io/provider).

## Rationale

The rationale for this design is detailed in the [Eio documentation](https://github.com/ocaml-multicore/eio/blob/main/doc/rationale.md#dynamic-dispatch).

## Motivation

The Provider library started an experimental project that extracted a pattern featured in the [Eio](https://github.com/ocaml-multicore/eio) project. The goal was to make this pattern reusable in other projects.

We then went on to use Provider as building block for the parametrization of early versions of the [Vcs](https://github.com/mbarbin/vcs) project.

However, after some time, we switched Vcs to a design using OCaml objects directly, and didn't make use of Provider after all (see [this vcs pr](https://github.com/mbarbin/vcs/pull/56)).

## Implementation

At its core, a provider is a pair consisting of an internal state and a virtual-table of first-class modules operating on that state. This allows dynamic dispatch, but for example doesn't include open recursion or inheritance in its execution model. This design offers an interesting balance between object-oriented and modular programming.

## Experimental Status

Please note that this library is highly experimental. The aim is to gain experience and feedback regarding whether this pattern can have larger applications outside of the scope of Eio.

## License

This project is licensed under the ISC license, the same as the original Eio project.

## Acknowledgements

We're very thankful to:
- The `Eio` developers for their work on the [Eio](https://github.com/ocaml-multicore/eio) project and for the documentation they published on the rationale for the design of the `Eio.Resource` module, which was the foundation of the Provider project.
- The [diataxis](https://diataxis.fr/) approach to technical documentation, which we use as inspiration to structure our doc.

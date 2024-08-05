# Design

Provider is an OCaml library for creating Traits and Signatures to build upon some functionality needed by a library without committing to an actual implementation for it, until runtime.

In essence, it provides a way to handle dynamic dispatch, where the target of a function call is not known until runtime. This is particularly useful in situations where there are many ways to provide a certain functionality, and the choice of provider is determined by the user at runtime.

## Rationale

The rationale for this design is detailed in the [Eio documentation](https://github.com/ocaml-multicore/eio/blob/main/doc/rationale.md#dynamic-dispatch).

## Implementation

At its core, a Provider is just a pair consisting of an internal state and a virtual-table of first-class modules operating on that state. This allows dynamic dispatch, but for example doesn't include open recursion or inheritance in its execution model. This design offers an interesting balance between object-oriented and modular programming.

## Motivation

The Provider library started as an experimental project that extracted a pattern featured in the [Eio](https://github.com/ocaml-multicore/eio) project. The goal was to make this pattern reusable in other projects.

We then went on to use Provider as building block for the parametrization of the [Vcs](https://mbarbin.github.io/vcs/) project.

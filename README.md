# provider

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

The Provider library started as an experimental project that extracted a pattern featured in the [Eio](https://github.com/ocaml-multicore/eio) project. The goal was to make this pattern reusable in other projects.

We then went on to use Provider as building block for the parametrization of the [Vcs](https://github.com/mbarbin/vcs) project.

## Implementation

At its core, a provider is a pair consisting of an internal state and a virtual-table of first-class modules operating on that state. This allows dynamic dispatch, but for example doesn't include open recursion or inheritance in its execution model. This design offers an interesting balance between object-oriented and modular programming.

## Experimental Status

Please note that this library is still considered experimental. The aim is to gain experience and feedback regarding whether this pattern can have larger applications outside of the scope of Eio.

## License

This project is licensed under the ISC license, the same as the original Eio project.

## Acknowledgements

We're very thankful to:
- The `Eio` developers for their work on the [Eio](https://github.com/ocaml-multicore/eio) project and for the documentation they published on the rationale for the design of the `Eio.Resource` module, which was the foundation of the Provider project.
- The [diataxis](https://diataxis.fr/) approach to technical documentation, which we use as inspiration to structure our doc.

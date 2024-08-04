# provider

[![CI Status](https://github.com/mbarbin/provider/workflows/ci/badge.svg)](https://github.com/mbarbin/provider/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/provider/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/provider?branch=main)

## Introduction

Provider is an OCaml library for creating Traits and Interfaces. It allows you to define the functionality needed by a library without committing to a specific implementation.

In essence, Provider handles dynamic dispatch, where the target of a function call is not known until runtime. This is particularly useful in situations where there are multiple ways to provide a certain functionality, and the choice of provider is determined by the end user.

This library started as an experimental project that extracted a pattern featured in the [Eio](https://github.com/ocaml-multicore/eio) project. The goal was to make this pattern reusable in other projects.

## Documentation

Published [here](https://mbarbin.github.io/provider).

## Rationale

The rationale for this design is detailed in the [Eio documentation](https://github.com/ocaml-multicore/eio/blob/main/doc/rationale.md#dynamic-dispatch).

## Implementation

At its core, a provider is just a pair consisting of an internal state and a virtual-table of first-class modules operating on that state. This allows dynamic dispatch, but for example doesn't include open recursion or inheritance in its execution model. This design offers an interesting balance between object-oriented and modular programming.

## Experimental Status

Please note that this library is still considered experimental. The aim is to gain experience and feedback regarding whether this pattern can have larger applications outside of the scope of Eio.

## License

This project is licensed under the ISC license, the same as the original Eio project.

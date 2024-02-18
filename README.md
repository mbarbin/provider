# provider

[![CI Status](https://github.com/mbarbin/provider/workflows/ci/badge.svg)](https://github.com/mbarbin/provider/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/provider/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/provider?branch=main)

## Introduction

The `provider` library is an experimental project that extracts and generalizes
a pattern used in the [Eio](https://github.com/ocaml-multicore/eio) project. The
goal is to make this pattern reusable in other projects.

The pattern involves using a "provider" construct that implements a set of
methods that an interface typically needs to provide certain functionality to a
client. This allows for manipulating values that behave like objects, without
using OCaml's built-in object system.

## Rationale

The rationale for this design is detailed in the [Eio documentation](https://github.com/ocaml-multicore/eio/blob/main/doc/rationale.md#dynamic-dispatch).

In essence, it provides a way to handle dynamic dispatch, where the target of a
function call is not known until runtime. This is particularly useful in
situations where there are many ways to provide a certain functionality, and the
choice of provider is determined by the user at runtime.

## Experimental Status

Please note that this library is still considered experimental. The aim is to
gain experience and feedback regarding whether this pattern can have larger
applications outside of the scope of Eio.

## License

This project is licensed under the ISC license, the same as the original Eio
project.

## Code documentation

The code documentation of the latest release is built with `odoc` and published
to `GitHub` pages [here](https://mbarbin.github.io/provider).

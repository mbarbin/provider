# Glossary

## Actors

**Library authors** identify the required functionality they want to parameterize over.

**Provider authors** implement these building blocks. There can be several different implementations of the same Traits, each with different characteristics (e.g., blocking/non-blocking, using/avoiding certain dependencies).

**Clients** (end users) write the final code that utilizes everything else. Clients decide which providers to use at runtime and instantiate the parameterized libraries with them.

## Operations

**Lookup**: A parameterized library performs a lookup at runtime in the provider structure supplied to it to find the implementation of the required Trait.

**Implement**: A provider *provides* an implementation for a given set of Traits.

**Instantiate**: A client chooses which provider to supply to their parameterized libraries to use them.

|     Actor        |   Operation   |
|------------------|:-------------:|
| Library authors  |    Lookup     |
| Provider authors |  Implement    |
| Clients          |  Instantiate  |

## Terminology

**Trait**: A module signature grouping some functionality.

**Implemenation**: A module that implements a Trait signature.

**Parameterized Library**: A library that requires the functionality contained in one or several Traits. It can compile without having access to actual Trait implementations.

**Binding**: The pair of a Trait and an actual implementation for it.

**Handler**: A list of bindings.

**Provider**: A handler coupled with its required internal state (an OCaml value that behaves like an object, but isn't one).

# Packaging

This guide shows how to architect the dependencies between different libraries involved in a typical provider setup.

- **Trait**: A library where you define your Traits and their signatures.
- **Library**: A library that is parameterized by your Traits.
- **Provider**: A library that implements your Traits.
- **Client**: An end user.

## Do

Here is the dependency graph you want to achieve (`dep -> a` means: library `a` depends on `dep`):

```mermaid
flowchart TD
    Trait --> Provider
    Trait --> Library
    Provider --> Client
    Library --> Client
```

The provider and library must agree on the interface the providers will need to implement. These signatures will be defined in some common dependency.

This scales well with the appearance of alternative providers and multiple clients, allowing them to limit their dependencies as needed:

```mermaid
flowchart TD
    Library --> Client1
    Trait --> Provider1
    Trait --> Provider2
    Trait --> Library
    Provider1 --> Client1
    Provider2 --> Client2
    Library --> Client2
```

## Or do

Alternatively, you may group the Traits definition with the library and have the provider implementation depend on the entire library.

```mermaid
flowchart TD
    Library["Library(+Trait)"] --> Provider
    Provider --> Client
    Library --> Client
```

This scales well too:

```mermaid
flowchart TD
    Library["Library(+Trait)"] --> Provider1
    Library["Library(+Trait)"] --> Provider2
    Provider1 --> Client1
    Provider2 --> Client2
    Library --> Client1
    Library --> Client2
```

## But don't do

What you usually want to avoid is for the library to inherit the dependencies of your providers.

```mermaid
flowchart TD
    Trait --> Provider
    Trait --> Library
    Provider --> Library
    Library --> Client
```

The point of a parameterized library is to limit your dependencies. You will run into trouble if all clients depend on all providers!

```mermaid
flowchart TD
    Trait --> Provider1
    Trait --> Library
    Trait --> Provider2

    Provider1 --> Library
    Provider2 --> Library
    Library --> Client1
    Library --> Client2
```

The `test/` directory is organized as follows:

- `interface/`

    This directory defines libraries that offer functionality to clients. These
    libraries include parametrized components that require the use of specific
    providers at runtime.

- `providers/`

    This directory defines providers for the interfaces mentioned above. These
    providers are used to instantiate the parametrized components of the
    libraries.

- `test__*`

    These files demonstrate how to use the interfaces defined by the libraries
    at runtime. This includes the instantiation of the parametrization with the
    use of specific providers.
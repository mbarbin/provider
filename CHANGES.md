## 0.0.10 (unreleased)

### Added

- Make the library build with `ocaml.4.14` (#22, @jonahbeckford, @mbarbin).
- Added new checks in CI (build checks on windows and macos) (#21, @mbarbin).
- Added a tutorial with new examples of handler-based polymorphism (#13, @mbarbin).

### Changed

- Make the library detect invalid Traits earlier (during handler creation) (#20, @mbarbin).
- Rename `explanation` section of documentation per diataxis.

### Deprecated

### Fixed

- Made the lib raise an exception instead of a segfault under some invalid usage (#18, @mbarbin).

### Removed

## 0.0.9 (2024-08-26)

### Changed

- Split test package.
- Use `expect_test_helpers_core.expect_test_helpers_base`.

### Fixed

- Attach doc test package so it is not built when running the CI for the main package.

## 0.0.8 (2024-08-19)

### Fixed

- Follow-up fixes to binding renaming.

## 0.0.7 (2024-08-05)

### Added

- Added dedicated tests for internal functions.

### Changed

- More renamings. `Implementation => Binding`, `Interface => Handler`. This brings the lib closer to the original namings from Eio.

## 0.0.6 (2024-08-02)

### Changed

- Rename `Provider.Trait.Implementation` as `Provider.Implementation` to expose the concepts in a more flat way.
- Reduce `provider` package dependencies - reduce from `base` to `sexplib0`.

### Fixed

- Make sure to select the right most implementation in case of overrides, as per specification.

### Removed

- Removed `Trait.Uid.Comparable.S` as this requires `Base`. Make it compatible with `Comparable.Make (Trait.Uid)` and add tests for this use case.

## 0.0.5 (2024-07-26)

### Added

- Added dependabot config for automatically upgrading action files.

### Changed

- Upgrade `ppxlib` to `0.33` - activate unused items warnings.
- Upgrade `ocaml` to `5.2`.
- Upgrade `dune` to `3.16`.
- Upgrade `eio` to `1.0` (no change required).
- Upgrade base & co to `0.17`.

## 0.0.4 (2024-03-05)

### Changed

- Uses `expect-test-helpers` (reduce core dependencies)
- Upgrade `eio` to `0.15`.
- Run `ppx_js_style` as a linter & make it a `dev` dependency.
- Upgrade GitHub workflows `actions/checkout` to v4.
- In CI, specify build target `@all`, and add `@lint`.
- List ppxs instead of `ppx_jane`.

## 0.0.3 (2024-02-21)

### Added

- Add new tests, improve test coverage.

### Changed

- Improve organization of test files.
- Rename `Class` => `Trait` (breaking change).

### Fixed

- Fix `Interface.implements` which shouldn't raise on empty interface (#3, @mbarbin).

## 0.0.2 (2024-02-18)

### Changed

- Improve tests and documentation.

## 0.0.1 (2024-02-18)

- Initial release.

## 0.0.6 (unreleased)

### Changed

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

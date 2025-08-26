(lang dune 3.20)

(name provider)

(generate_opam_files)

(license ISC)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/provider))

(documentation "https://mbarbin.github.io/provider/")

(using mdx 0.4)

(implicit_transitive_deps false-if-hidden-includes-supported)

(package
 (name provider)
 (synopsis "Dynamic Dispatch with Traits")
 (depends
  (ocaml
   (>= 4.14))
  (sexplib0
   (>= v0.16))))

(package
 (name provider-tests)
 (synopsis "Tests for provider")
 (depends
  (ocaml
   (>= 4.14))
  (base
   (>= v0.16))
  (expect_test_helpers_core
   (>= v0.16))
  (ppx_compare
   (>= v0.16))
  (ppx_enumerate
   (>= v0.16))
  (ppx_expect
   (>= v0.16))
  (ppx_hash
   (>= v0.16))
  (ppx_here
   (>= v0.16))
  (ppx_let
   (>= v0.16))
  (ppx_sexp_conv
   (>= v0.16))
  (ppx_sexp_value
   (>= v0.16))
  (ppxlib
   (>= 0.33))
  (provider
   (= :version))))

(package
 (name provider-dev)
 (synopsis
  "Package to regroup dev targets for the provider project, documentation, and more")
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (base
   (>= v0.17))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8.3)))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (expect_test_helpers_core
   (>= v0.17))
  (mdx
   (>= 2.4))
  (ppx_compare
   (>= v0.17))
  (ppx_enumerate
   (>= v0.17))
  (ppx_expect
   (>= v0.17))
  (ppx_hash
   (>= v0.17))
  (ppx_here
   (>= v0.17))
  (ppx_js_style
   (and
    :with-dev-setup
    (>= v0.17)))
  (ppx_let
   (>= v0.17))
  (ppx_sexp_conv
   (>= v0.17))
  (ppx_sexp_value
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (provider
   (= :version))
  (provider-tests
   (= :version))
  (sherlodoc
   (and
    :with-doc
    (>= 0.2)))))

(lang dune 3.17)

(name provider)

(generate_opam_files)

(license ISC)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/provider))

(documentation "https://mbarbin.github.io/provider/")

(using mdx 0.4)

(implicit_transitive_deps false)

(package
 (name provider)
 (synopsis "Dynamic Dispatch with Traits")
 (depends
  (ocaml
   (>= 4.14))
  (sexplib0
   (and
    (>= v0.16)
    (< v0.18)))))

(package
 (name provider-tests)
 (synopsis "Tests for provider")
 (depends
  (ocaml
   (>= 4.14))
  (base
   (and
    (>= v0.16)
    (< v0.18)))
  (expect_test_helpers_core
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_compare
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_enumerate
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_expect
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_here
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_let
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.16)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.16)
    (< v0.18)))
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
   (and
    (>= v0.17)
    (< v0.18)))
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.8.3)))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (expect_test_helpers_core
   (and
    (>= v0.17)
    (< v0.18)))
  (mdx
   (>= 2.4))
  (ppx_compare
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_enumerate
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_expect
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_hash
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_here
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_js_style
   (and
    :with-dev-setup
    (>= v0.17)
    (< v0.18)))
  (ppx_let
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_conv
   (and
    (>= v0.17)
    (< v0.18)))
  (ppx_sexp_value
   (and
    (>= v0.17)
    (< v0.18)))
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

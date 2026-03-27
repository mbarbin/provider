(lang dune 3.20)

(name provider)

(generate_opam_files)

(license ISC)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin <opensource@mbarbin.org>")

(source
 (github mbarbin/provider))

(documentation "https://mbarbin.github.io/provider/")

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
  (dyn
   (>= 3.20))
  (ordering
   (>= 3.20))
  (ppx_expect
   (>= v0.16))
  (pp
   (>= 2.0.0))
  (ppxlib
   (>= 0.33))
  (provider
   (= :version))
  (sexplib0
   (>= v0.16))))

(package
 (name provider-eio-tests)
 (synopsis "Tests for provider using Eio")
 (depends
  (ocaml
   (>= 5.2))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (ppx_expect
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (provider
   (= :version))
  (provider-tests
   (= :version))))

(package
 (name provider-dev)
 (synopsis "Package to regroup dev targets, documentation, and more")
 (allow_empty)
 (depends
  (ocaml
   (>= 5.2))
  (ocamlformat
   (= 0.29.0))
  (bisect_ppx
   (>= 2.8.3))
  (dyn
   (>= 3.20))
  (pp
   (>= 2.0.0))
  (ppx_expect
   (>= v0.17))
  (ppx_js_style
   (>= v0.17))
  (ppxlib
   (>= 0.33))
  (provider
   (= :version))
  (provider-tests
   (= :version))
  (sherlodoc
   (>= 0.2))))

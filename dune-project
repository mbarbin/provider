(lang dune 3.16)

(name provider)

(generate_opam_files)

(license ISC)

(authors "Mathieu Barbin")

(maintainers "Mathieu Barbin")

(source
 (github mbarbin/provider))

(documentation "https://mbarbin.github.io/provider/")

(using mdx 0.4)

(package
 (name provider)
 (synopsis "Dynamic Dispatch with Traits")
 (depends
  (ocaml
   (>= 5.2))
  (bisect_ppx
   (and
    :dev
    (>= 2.8.3)))
  (ppx_js_style
   (and
    :dev
    (>= v0.17)
    (< v0.18)))
  (sexplib0
   (and
    (>= v0.17)
    (< v0.18)))))

(package
 (name provider-test)
 (synopsis "Test suite for provider library")
 (depends
  (ocaml
   (>= 5.2))
  (base
   (and
    (>= v0.17)
    (< v0.18)))
  (bisect_ppx
   (and
    :dev
    (>= 2.8.3)))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (expect-test-helpers
   (and
    (>= v0.17)
    (< v0.18)))
  (mdx
   (and
    :with-doc
    (>= 2.4)))
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
    :dev
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
   (= :version))))

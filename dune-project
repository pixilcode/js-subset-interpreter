(lang dune 3.13)

(name js_interpreter)

(generate_opam_files true)

(source
 (github pixilcode/cs330_js_interpreter_ocaml))

(authors "Brendon Bown")

(package
 (name js_interpreter)
 (synopsis "An interpreter for a JavaScript-like language, written for CS 330")
 (depends
  ocaml
  dune))

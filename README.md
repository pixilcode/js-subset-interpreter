# JavaScript Interpreter

## System Requirements
* Install OCaml
  * [Unix and macOS](https://ocaml.org/docs/installing-ocaml#installation-on-unix-and-macos)
  * [Windows](https://ocaml.org/docs/installing-ocaml#installation-on-windows)
* Install `dune`
  * `opam install dune`
* Install `acorn`
  * `npm install -g acorn` (`-g` implies that it will be installed globally,
    excluding `-g` will install it locally)


## Running the parser

For a file containing JavaScript code, let's say `foo.js`, run the following:

```bash
acorn --ecma2024 foo.js | dune exec print_ast
```


## Running the interpreter

For a file containing JavaScript code, let's say `foo.js`, run the following:

```bash
acorn --ecma2024 foo.js | dune exec interpret
```


## JavaScript Subset

This interpreter interprets a subset of JavaScript including:
- booleans (`true`, `false`)
- numbers (`1`, `2.3`)
- unary operators
  - boolean (`!`)
  - number (`+`, `-`)
- binary operators
  - relational (`==`, `<`)
  - arithmetic (`+`, `-`, `*`, `/`)
  - logical (`&&`, `||`)
- conditional expressions (`true ? 1 : 2`)
- variable declaration (`let x = 1;`)
- variable reassignment expression (`x = 2`)
- single-argument function expressions (`function (x) { return x }`)
- single-argument function calls (`foo(1)`)

For more details, see `test/test_js_print_ast.ml` and `test/test_js_interpret.ml`.

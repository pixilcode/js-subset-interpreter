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


## Running the program

For a file containing JavaScript code, let's say `foo.js`, run the following:

```bash
acorn --ecma2024 foo.js | dune exec print_ast
```
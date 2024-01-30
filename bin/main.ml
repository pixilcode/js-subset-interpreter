open Js_interpreter

(* read stdin into a string *)
let () =
  let rec read_stdin () =
    try
      let line = input_line stdin in
      line ^ "\n" ^ read_stdin ()
    with
      End_of_file -> ""
  in
  let input = read_stdin () in
  let ast = Ast.from_json_string input in
  let s_expr = S_expr.from_ast ast in
  print_endline (S_expr.pretty_print s_expr)

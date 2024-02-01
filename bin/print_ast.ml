open Js_interpreter

(* read stdin into a string *)
let () =
  let input = Util.read_stdin () in
  let ast = Ast.from_json_string input in
  let s_expr = S_expr.from_ast ast in
  print_endline (S_expr.to_string s_expr)

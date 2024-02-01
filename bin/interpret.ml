open Js_interpreter

let () =
  let input = Util.read_stdin () in
  let ast = Ast.from_json_string input in
  let result = Interpreter.interpret ast in
  let s_expr = S_expr.from_result result in
  print_endline (S_expr.to_string s_expr)
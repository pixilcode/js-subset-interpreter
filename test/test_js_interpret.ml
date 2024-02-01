open Js_interpreter

(* run a test and print the result *)
let run_test (name, js) =
  print_endline ("--- " ^ name ^ " ---");

  let json = Util.run_acorn js in
  let ast = Ast.from_json_string json in
  let result = Interpreter.interpret ast in
  let s_expr = S_expr.from_result result in

  print_endline (S_expr.to_string s_expr);
  print_newline ()

let tests = [
  ("simple_number", "1");
  ("unary", "!true");
  ("arithmetic", "1 + 2 - 3 * 4 / 6");
  ("relational", "1 < 2 == true");
  ("logical", "true && false || false")
  (* TODO: write tests that produce errors *)
]

(* iterate over all the tests and run them *)
let () = List.iter run_test tests
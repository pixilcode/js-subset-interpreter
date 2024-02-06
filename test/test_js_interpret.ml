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
  ("relational", "1 < 2");
  ("equality", "1 == 2");
  ("logical", "true && false || false");
  ("short_circuit_or_logic", "1 == 1 || true + false");
  ("short_circuit_and_logic", "1 == 2 && true + false");
  ("error_boolean_1", "1 == 2 || 1 + 2");
  ("error_boolean_2", "1 == 1 && 1 + 2");
  ("error_boolean_3", "1 && true");
  ("error_boolean_4", "false || 3");
  ("error_arithmetic_1", "1 + true");
  ("error_arithmetic_2", "true - 1");
  ("error_arithmetic_3", "false * true");
  ("error_arithmetic_4", "true / true");
  ("error_unary_1", "!1");
  ("error_unary_2", "-true");
  ("error_unary_3", "+false");
  ("error_conditional", "1 ? 2 : 3");
  ("error_divide_by_zero", "6 / 0")
  (* TODO: write tests that produce errors *)
]

(* iterate over all the tests and run them *)
let () = List.iter run_test tests
open Js_interpreter

(* run a test and print the result *)
let run_test (name, js) =
  print_endline ("--- " ^ name ^ " ---");

  let json = Util.run_acorn js in
  let ast = Parser.from_json_string json in
  let s_expr = S_expr.from_ast ast in

  print_endline (S_expr.to_string s_expr);
  print_newline ()

let tests = [
  ("simple_number", "1");
  ("unary", "!true");
  ("arithmetic", "1 + 2 - 3 * 4 / 5");
  ("relational", "1 < 2 == true");
  ("logical", "true && false || false")
]

(* iterate over all the tests and run them *)
let () = List.iter run_test tests
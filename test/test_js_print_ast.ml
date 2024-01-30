open Js_interpreter

let run_acorn js =
  (* write the js to a temp file *)
  let temp_js_file = Filename.temp_file "acorn" ".js" in
  let temp_json_file = Filename.temp_file "acorn" ".json" in
  let oc = open_out temp_js_file in
  output_string oc js;
  close_out oc;

  (* run acorn on the temp file *)
  ignore (Sys.command ("acorn --ecma2024 " ^ temp_js_file ^ " > " ^ temp_json_file) : int);

  (* read the result *)
  let ic = open_in_bin temp_json_file in
  let json = really_input_string ic (in_channel_length ic) in
  close_in ic;

  (* delete the temp files *)
  Sys.remove temp_js_file;
  Sys.remove temp_json_file;

  json

(* run a test and print the result *)
let run_test (name, js) =
  print_endline ("--- " ^ name ^ " ---");

  let json = run_acorn js in
  let ast = Ast.from_json_string json in
  let s_expr = S_expr.from_ast ast in

  print_endline (S_expr.pretty_print s_expr);
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
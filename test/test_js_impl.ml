open Js_impl

let run_acorn js =
  (* write the js to a temp file *)
  let temp_js_file = Filename.temp_file "acorn" ".js" in
  let temp_json_file = Filename.temp_file "acorn" ".json" in
  let oc = open_out temp_js_file in
  output_string oc js;
  close_out oc;

  print_endline ("temp js file: " ^ temp_js_file);
  print_endline ("temp json file: " ^ temp_json_file);

  (* run acorn on the temp file *)
  ignore (Sys.command ("acorn --ecma2024 " ^ temp_js_file ^ " > " ^ temp_json_file) : int);

  (* read the result *)
  let ic = open_in temp_json_file in
  let json = input_line ic in
  close_in ic;

  (* delete the temp files *)
  Sys.remove temp_js_file;
  Sys.remove temp_json_file;

  json

(* run a test and print the result *)
let run_test (name, js) =
  print_endline ("--- " ^ name ^ " ---");
  let json = run_acorn js in
  let ast = Ast.from_json json in
  let s_expr = S_expr.from_ast ast in
  print_endline (S_expr.pretty_print s_expr)

let tests = [
  ("simple_number", "1")
]

(* iterate over all the tests and run them *)
let () = List.iter run_test tests
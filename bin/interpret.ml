open Js_interpreter

let () =
  let input = Util.read_stdin () in
  let ast = Parser.from_json_string input in
  let result = Interpreter.interpret ast in
  match result with
  | Ok (values, _env) ->
    let values = List.map S_expr.from_value values in
    let values = List.map S_expr.to_string values in
    List.iter print_endline values;
    print_newline ()
  | Error message ->
    let message = S_expr.from_error message in
    let message = S_expr.to_string message in
    print_endline message;
    print_newline ()
let from_json_string json =
  let open Yojson.Safe.Util in
  let open Ast in

  let fail_parsing error_message json =
    Yojson.Safe.pp Format.err_formatter json;
    prerr_newline ();
    prerr_newline ();
    failwith ("Banana cream pies! " ^ error_message)
  in

  let get_node_type json =
    json |> member "type" |> to_string
  in

  let node_is_type json type_ =
    get_node_type json = type_
  in

  let rec expression_from_json json =
    if node_is_type json "Literal" then
      match json |> member "value" with
      | `Int value -> Expression.Literal (Literal.Number value)
      | `Bool value -> Expression.Literal (Literal.Boolean value)
      | _ -> fail_parsing "Invalid literal!" json
    else if node_is_type json "UnaryExpression" then
      let operator = json |> member "operator" |> to_string in
      let argument = json |> member "argument" in
      let argument = expression_from_json argument in
      match operator with
      | "!" -> Unary (Negate_bool, argument)
      | "-" -> Unary (Negate_number, argument)
      | "+" -> Unary (Positive, argument)
      | _ -> fail_parsing ("Invalid unary operator '" ^ operator ^ "'!") json
    else if node_is_type json "BinaryExpression" then
      let operator = json |> member "operator" |> to_string in
      let left = json |> member "left" in
      let left = expression_from_json left in
      let right = json |> member "right" in
      let right = expression_from_json right in
      match operator with
      | "+" -> Binary (left, Plus, right)
      | "-" -> Binary (left, Minus, right)
      | "*" -> Binary (left, Times, right)
      | "/" -> Binary (left, Divide, right)
      | "==" -> Binary (left, Equal, right)
      | "<" -> Binary (left, Less_than, right)
      | _ -> fail_parsing ("Invalid binary operator '" ^ operator ^ "'!") json
    else if node_is_type json "LogicalExpression" then
      let operator = json |> member "operator" |> to_string in
      let left = json |> member "left" in
      let left = expression_from_json left in
      let right = json |> member "right" in
      let right = expression_from_json right in
      match operator with
      | "&&" -> Logical (left, And, right)
      | "||" -> Logical (left, Or, right)
      | _ -> fail_parsing ("Invalid logical operator '" ^ operator ^ "'!") json
    else if node_is_type json "ConditionalExpression" then
      let test = json |> member "test" in
      let test = expression_from_json test in
      let consequent = json |> member "consequent" in
      let consequent = expression_from_json consequent in
      let alternate = json |> member "alternate" in
      let alternate = expression_from_json alternate in
      Conditional (test, consequent, alternate)
    else if node_is_type json "Identifier" then
      let name = json |> member "name" |> to_string in
      Identifier name
    else if node_is_type json "FunctionExpression" then
      let params = json |> member "params" |> to_list in
      let num_params = List.length params in
      if num_params <> 1 then
        let num_params_string = string_of_int num_params in
        fail_parsing (
          "Function must have one parameter, function has " ^
          num_params_string ^
          " parameters!"
        ) json
      else
        let param = List.hd params in
        let param_name = param |> member "name" |> to_string in
        (* this assumes that the body is a block statement *)
        let body = json |> member "body" |> member "body" |> to_list in
        let body = List.map statement_from_json body in
        Expression.Function (param_name, body)
    else if node_is_type json "CallExpression" then
      let callee = json |> member "callee" in
      let callee = expression_from_json callee in
      let arguments = json |> member "arguments" |> to_list in
      let num_arguments = List.length arguments in
      if num_arguments <> 1 then
        let num_arguments_string = string_of_int num_arguments in
        fail_parsing (
          "Function call must have one argument, call has " ^
          num_arguments_string ^
          " arguments!"
        ) json
      else
        let arg = List.hd arguments in
        let arg = expression_from_json arg in
        Expression.Call (callee, arg)
    else if node_is_type json "AssignmentExpression" then
      let ident =
        json
        |> member "left"
        |> to_string
      in
      let expression =
        json
        |> member "right"
        |> expression_from_json
      in
      Expression.Assignment (ident, expression)
    else
      fail_parsing "Invalid expression!" json

  and declaration_from_json json =
    if node_is_type json "VariableDeclarator" then
      let ident =
        json
        |> member "id"
        |> member "name"
        |> to_string
      in
      let expr_json = json |> member "init" in
      let expr = expression_from_json expr_json in
      (ident, expr)
    else
      fail_parsing "Invalid declaration!" json

  and statement_from_json json =
    if node_is_type json "ExpressionStatement" then
      let expression = json |> member "expression" in
      let expression = expression_from_json expression in
      Statement.Expression_statement expression
    else if node_is_type json "VariableDeclaration" then
      let declarations = json |> member "declarations" |> to_list in
      let declarations = List.map declaration_from_json declarations in
      Statement.Variable_declaration declarations
    else if node_is_type json "ReturnStatement" then
      let argument = json |> member "argument" in
      let argument = expression_from_json argument in
      Statement.Return_statement argument
    else if node_is_type json "BlockStatement" then
      let body = json |> member "body" |> to_list in
      let body = List.map statement_from_json body in
      Statement.Block_statement body
    else
      fail_parsing "Invalid statement!" json
  in

  let program_from_json json =
    if node_is_type json "Program" then
      let statements = json |> member "body" |> to_list in
      let statements = List.map statement_from_json statements in
      Program.Program statements
    else
      fail_parsing "Invalid program!" json
  in

  let json = Yojson.Safe.from_string json in
  program_from_json json
  
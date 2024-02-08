let from_json_string json =
  let open Yojson.Basic.Util in
  let open Ast in

  let get_node_type json =
    let open Yojson.Basic.Util in
    json |> member "type" |> to_string
  in

  let node_is_type json type_ =
    get_node_type json = type_
  in

  let rec expression_from_json json =
    let open Yojson.Basic.Util in
    if node_is_type json "Literal" then
      match json |> member "value" with
      | `Int value -> Expression.Literal (Literal.Number value)
      | `Bool value -> Expression.Literal (Literal.Boolean value)
      | _ -> failwith "Invalid JSON"
    else if node_is_type json "UnaryExpression" then
      let operator = json |> member "operator" |> to_string in
      let argument = json |> member "argument" in
      let argument = expression_from_json argument in
      match operator with
      | "!" -> Unary (Negate_bool, argument)
      | "-" -> Unary (Negate_number, argument)
      | "+" -> Unary (Positive, argument)
      | _ -> failwith "Invalid JSON"
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
      | _ -> failwith "Invalid JSON"
    else if node_is_type json "LogicalExpression" then
      let operator = json |> member "operator" |> to_string in
      let left = json |> member "left" in
      let left = expression_from_json left in
      let right = json |> member "right" in
      let right = expression_from_json right in
      match operator with
      | "&&" -> Logical (left, And, right)
      | "||" -> Logical (left, Or, right)
      | _ -> failwith "Invalid JSON"
    else if node_is_type json "ConditionalExpression" then
      let test = json |> member "test" in
      let test = expression_from_json test in
      let consequent = json |> member "consequent" in
      let consequent = expression_from_json consequent in
      let alternate = json |> member "alternate" in
      let alternate = expression_from_json alternate in
      Conditional (test, consequent, alternate)
    else
      failwith "Invalid JSON"
  in

  let statement_from_json json =
    if node_is_type json "ExpressionStatement" then
      let expression = json |> member "expression" in
      let expression = expression_from_json expression in
      Statement.Expression_statement expression
    else
      failwith "Invalid JSON"
  in

  let program_from_json json =
    if node_is_type json "Program" then
      let statements = json |> member "body" |> to_list in
      let first_statement = List.hd statements in
      let first_statement = statement_from_json first_statement in
      Program.Program first_statement
    else
      failwith "Invalid JSON"
  in

  let json = Yojson.Basic.from_string json in
  program_from_json json
  
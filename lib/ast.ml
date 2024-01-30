let get_node_type json =
  let open Yojson.Basic.Util in
  json |> member "type" |> to_string

let node_is_type json type_ =
  get_node_type json = type_

module Literal = struct
  type t =
    | Number of int
    | Boolean of bool
end

module Binary_operator = struct
  type t =
    | Plus
    | Minus
    | Times
    | Divide
    | Equal
    | Less_than
  
  let to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Equal -> "=="
    | Less_than -> "<"
end

module Unary_operator = struct
  type t =
    | Negate_bool
    | Negate_number
    | Positive
  
  let to_string = function
    | Negate_bool -> "!"
    | Negate_number -> "-"
    | Positive -> "+"
end

module Logical_operator = struct
  type t =
    | And
    | Or
  
  let to_string = function
    | And -> "&&"
    | Or -> "||"
end

module Expression = struct
  type t =
    | Literal of Literal.t
    | Binary of t * Binary_operator.t * t
    | Unary of Unary_operator.t * t
    | Logical of t * Logical_operator.t * t
    | Conditional of t * t * t

  let rec from_json json =
    let open Yojson.Basic.Util in
    if node_is_type json "Literal" then
      match json |> member "value" with
      | `Int value -> Literal (Number value)
      | `Bool value -> Literal (Boolean value)
      | _ -> failwith "Invalid JSON"
    else if node_is_type json "UnaryExpression" then
      let operator = json |> member "operator" |> to_string in
      let argument = json |> member "argument" in
      let argument = from_json argument in
      match operator with
      | "!" -> Unary (Negate_bool, argument)
      | "-" -> Unary (Negate_number, argument)
      | "+" -> Unary (Positive, argument)
      | _ -> failwith "Invalid JSON"
    else if node_is_type json "BinaryExpression" then
      let operator = json |> member "operator" |> to_string in
      let left = json |> member "left" in
      let left = from_json left in
      let right = json |> member "right" in
      let right = from_json right in
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
      let left = from_json left in
      let right = json |> member "right" in
      let right = from_json right in
      match operator with
      | "&&" -> Logical (left, And, right)
      | "||" -> Logical (left, Or, right)
      | _ -> failwith "Invalid JSON"
    else if node_is_type json "ConditionalExpression" then
      let test = json |> member "test" in
      let test = from_json test in
      let consequent = json |> member "consequent" in
      let consequent = from_json consequent in
      let alternate = json |> member "alternate" in
      let alternate = from_json alternate in
      Conditional (test, consequent, alternate)
    else
      failwith "Invalid JSON"
end

module Statement = struct
  type t = Expression_statement of Expression.t

  let from_json json =
    let open Yojson.Basic.Util in
    if node_is_type json "ExpressionStatement" then
      let expression = json |> member "expression" in
      let expression = Expression.from_json expression in
      Expression_statement expression
    else
      failwith "Invalid JSON"
end

module Program = struct
  type t = Program of Statement.t

  let from_json json =
    let open Yojson.Basic.Util in
    if node_is_type json "Program" then
      let statements = json |> member "body" |> to_list in
      let first_statement = List.hd statements in
      let first_statement = Statement.from_json first_statement in
      Program first_statement
    else
      failwith "Invalid JSON"
end

let from_json_string json =
  let json = Yojson.Basic.from_string json in
  Program.from_json json
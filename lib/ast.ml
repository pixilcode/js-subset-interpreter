let get_node_type json =
  let open Yojson.Basic.Util in
  json |> member "type" |> to_string

let node_is_type json type_ =
  get_node_type json = type_

module Literal = struct
  type t =
    | Number of float
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

  let from_json json =
    let open Yojson.Basic.Util in
    if node_is_type json "Literal" then
      match json |> member "value" with
      | `Float value -> Literal (Number value)
      | `Int value -> Literal (Number (float_of_int value))
      | `Bool value -> Literal (Boolean value)
      | _ -> failwith "Invalid JSON"
    else
      failwith "Not implemented"
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

let from_json json =
  let json = Yojson.Basic.from_string json in
  Program.from_json json
type t =
  | Expr of t list
  | Atom of string

let from_ast ast =
  let open Ast in

  let (
    boolean_keyword,
    number_keyword,
    unary_keyword,
    arithmetic_keyword,
    relational_keyword,
    logical_keyword,
    conditional_keyword
  ) = (
    Atom "boolean",
    Atom "number",
    Atom "unary",
    Atom "arithmetic",
    Atom "relational",
    Atom "logical",
    Atom "conditional"
  ) in

  let from_literal = function
    | Literal.Number n -> Expr [number_keyword; Atom (string_of_int n)]
    | Literal.Boolean b -> Expr [boolean_keyword; Atom (string_of_bool b)]
  in  
  
  let rec from_expression = function
    | Expression.Literal l -> from_literal l
    | Expression.Binary (lhs, op, rhs) ->
      let keyword = match op with
        | Binary_operator.Plus
        | Binary_operator.Minus
        | Binary_operator.Times
        | Binary_operator.Divide -> arithmetic_keyword
        | Binary_operator.Equal
        | Binary_operator.Less_than -> relational_keyword
      in
      let op = Binary_operator.to_string op in
      let op = Atom op in
      let lhs = from_expression lhs in
      let rhs = from_expression rhs in
      Expr [keyword; op; lhs; rhs]
    | Expression.Unary (op, rhs) ->
      let op = Unary_operator.to_string op in
      let op = Atom op in
      let rhs = from_expression rhs in
      Expr [unary_keyword; op; rhs]
    | Expression.Logical (lhs, op, rhs) ->
      let op = Logical_operator.to_string op in
      let op = Atom op in
      let lhs = from_expression lhs in
      let rhs = from_expression rhs in
      Expr [logical_keyword; op; lhs; rhs]
    | Expression.Conditional (cond, then_, else_) ->
      let cond = from_expression cond in
      let then_ = from_expression then_ in
      let else_ = from_expression else_ in
      Expr [conditional_keyword; cond; then_; else_]
  in

  let from_statement = function
    | Statement.Expression_statement e -> from_expression e
  in

  let from_program = function
    | Program.Program first_statement -> from_statement first_statement
  in
  from_program ast

let rec pretty_print s_expr =
  match s_expr with
  | Expr l -> "(" ^ String.concat " " (List.map pretty_print l) ^ ")"
  | Atom s -> s
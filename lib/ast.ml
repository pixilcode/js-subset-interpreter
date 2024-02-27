module Identifier = struct
  type t = string
end

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
  type 'stmt t =
    | Literal of Literal.t
    | Identifier of Identifier.t
    | Binary of 'stmt t * Binary_operator.t * 'stmt t
    | Unary of Unary_operator.t * 'stmt t
    | Logical of 'stmt t * Logical_operator.t * 'stmt t
    | Conditional of 'stmt t * 'stmt t * 'stmt t
    | Call of 'stmt t * 'stmt t
    | Function of Identifier.t * ('stmt list)
end

module Statement = struct
  type t =
    | Expression_statement of t Expression.t
    | Variable_declaration of (Identifier.t * t Expression.t) list
    | Return_statement of t Expression.t
    | Block_statement of t list
end

module Program = struct
  type t = Program of Statement.t list
end
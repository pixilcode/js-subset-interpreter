module Literal = struct
  type t = Number of float | Boolean of bool
end

module Expression = struct
  type t = Literal of Literal.t
end

module Statement = struct
  type t = Expression_statement of Expression.t
end

module Program = struct
  type t = Program of Statement.t
end

let from_json _json = failwith "Not implemented"
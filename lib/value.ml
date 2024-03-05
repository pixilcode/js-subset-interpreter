type t =
| Number of int
| Boolean of bool
| Function of string * Ast.Statement.t list * t Env.t
| Void
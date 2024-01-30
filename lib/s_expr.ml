type t =
  | Expr of t list
  | Atom of string

let from_ast _ast = failwith "Not implemented"

let rec pretty_print s_expr =
  match s_expr with
  | Expr l -> "(" ^ String.concat " " (List.map pretty_print l) ^ ")"
  | Atom s -> s
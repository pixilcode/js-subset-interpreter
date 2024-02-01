let rec interpret ast =
  let open Ast in
  let eval_literal l =
    match l with
    | Literal.Number i -> Ok (Value.Number i)
    | Literal.Boolean b -> Ok (Value.Boolean b)
  in

  let rec eval_expression e =
    match e with
    | Expression.Literal l -> eval_literal l
    | Expression.Binary (lhs, op, rhs) ->
      failwith "Unimplemented"
      (*
      let lhs = eval_expression lhs in
      let rhs = eval_expression rhs in
      let open Binary_operator in
      match op with
      | Plus -> lhs + rhs
      | Minus -> lhs + rhs
      *)
    | Expression.Logical (lhs, op, rhs) ->
      failwith "Unimplemented"
    | Expression.Unary (op, rhs) ->
      failwith "Unimplemented"
    | Expression.Conditional (cond, if_t, if_f) ->
      failwith "Unimplemented"
  in

  failwith "Unimplemented"

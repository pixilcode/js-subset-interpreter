open Core

let error msg =
  Error ("Oh bananas! " ^ msg)

let interpret ast =
  let open Ast in
  let open Core.Result.Monad_infix in
  let eval_literal l: Value.t =
    match l with
    | Literal.Number i -> Value.Number i
    | Literal.Boolean b -> Value.Boolean b
  in

  let rec eval_expression e env: (Value.t * Value.t Env.t, string) result =
    match e with
    | Expression.Literal l -> Ok (eval_literal l, env)
    | Expression.Binary (lhs, op, rhs) -> begin
      eval_expression lhs env >>= fun (lhs, env) ->
      eval_expression rhs env >>= fun (rhs, env) ->
      match (lhs, rhs) with
      | (Value.Number lhs, Value.Number rhs) -> begin
        let open Binary_operator in
        match op with
        | Plus -> Ok (Value.Number (lhs + rhs), env)
        | Minus -> Ok (Value.Number (lhs - rhs), env)
        | Times -> Ok (Value.Number (lhs * rhs), env)
        | Divide ->
          if rhs = 0 then
            error "Cannot divide by zero"
          else
            Ok (Value.Number (lhs / rhs), env)
        | Equal -> Ok (Value.Boolean (lhs = rhs), env)
        | Less_than -> Ok (Value.Boolean (lhs < rhs), env)
      end
      | _ -> error "Invalid arithmetic operation" (* TODO: better error message *)
    end
    | Expression.Logical (lhs, op, rhs) -> begin
      eval_expression lhs env >>= fun (lhs, env) ->
      let open Logical_operator in
      match lhs, op with
      (* short-circuit boolean operators *)
      | Value.Boolean false, And
      | Value.Boolean true, Or -> Ok (lhs, env)
      (* evaluate second argument*)
      | Value.Boolean true, And
      | Value.Boolean false, Or -> begin
        eval_expression rhs env >>= fun (rhs, env) ->
        match rhs with 
        | Value.Boolean _ -> Ok (rhs, env)
        | _ -> error "Invalid logical operation" (* TODO: better error message *)
      end
      (* invalid operators *)
      | _ -> error "Invalid logical operation" (* TODO: better error message *)
    end
    | Expression.Unary (op, rhs) -> begin
      eval_expression rhs env >>= fun (rhs, env) ->
      let open Unary_operator in
      match (op, rhs) with
      | (Positive, Value.Number rhs) -> Ok (Value.Number (-rhs), env)
      | (Negate_number, Value.Number rhs) -> Ok (Value.Number (rhs), env)
      | (Negate_bool, Value.Boolean rhs) -> Ok (Value.Boolean (not rhs), env)
      | _ -> error "Invalid unary operation" (* TODO: better error message *)
      end
    | Expression.Conditional (cond, if_t, if_f) -> begin
      eval_expression cond env >>= fun (cond, env) ->
      match cond with
      | Value.Boolean cond ->
        if cond then
          eval_expression if_t env
        else
          eval_expression if_f env
      | _ -> error "Invalid condition in conditional"
    end
    | Expression.Identifier ident ->
      let value = Env.get ~ident env in
      let ident_error = error "Undefined identifier" in
      Option.value_map value ~default:ident_error ~f:(
        fun value -> Ok (value, env)
      )
    | Expression.Call (fn, arg) -> begin
      eval_expression fn env >>= fun (fn, env) ->
      eval_expression arg env >>= fun (arg, env) ->
      eval_function_call fn arg >>= fun result ->
      Ok (result, env)
    end
    | Expression.Function (arg_name, body) ->
      let function_value = Value.Function (arg_name, body, Env.copy env) in
      Ok (function_value, env)
    | Expression.Assignment (_ident, _expression) ->
      failwith "unimplemented"

  and eval_statement s env: (Value.t option * Value.t Env.t, string) result =
    match s with
    | Statement.Expression_statement e ->
      eval_expression e env >>| fun (value, env) ->
      (Some value, env)
    | Statement.Variable_declaration decls ->
      let env = List.fold decls ~init:(Ok env) ~f:(fun env (ident, expr) ->
        env >>= fun env ->
        eval_expression expr env >>= fun (value, env) ->
        let env = Env.set ~ident ~value env in
        Ok (env)
      ) in
      env >>| fun env ->
      (None, env)
    | Statement.Block_statement stmts ->
      eval_statement_list stmts env >>| fun (_results, env) ->
      (* a block doesn't return its results *)
      (None, env)
    | Statement.Return_statement _expression ->
      error "Return statement outside of a function"
    
  and eval_function_call fn arg: (Value.t, string) result =
    match fn with
    | Value.Function (arg_name, body, env) ->
      let env =
        Env.with_parent env
        |> Env.set ~ident:arg_name ~value:arg
      in
      eval_function_body body env
    | _ -> error "Non-function item cannot be called"
  
  and eval_function_body stmts env: (Value.t, string) result =
    match stmts with
    | [] -> Ok Value.Void
    | (Statement.Return_statement return_expr) :: _rest ->
      eval_expression return_expr env >>= fun (return_value, _env) ->
        Ok return_value
    | stmt :: stmts_rest ->
      eval_statement stmt env >>= fun (_output, env) ->
      eval_function_body stmts_rest env
  
  and eval_statement_list stmts env: (Value.t list * Value.t Env.t, string) result =
    List.fold stmts ~init:(Ok ([], env)) ~f:(fun prev_result s ->
      prev_result >>= fun (prev_outputs, env) ->
      eval_statement s env >>= fun (output, env) ->
      let output = Option.to_list output in
      Ok (prev_outputs @ output, env)
    )
  in
  
  let eval_program p env: (Value.t list * Value.t Env.t, string) result =
    match p with
    | Program.Program stmts ->
      eval_statement_list stmts env
  in
  
  eval_program ast (Env.empty ())

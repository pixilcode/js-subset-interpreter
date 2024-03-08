open Core

type env = Env.t
type heap = Value.t Heap.t
type value = Value.t

let error msg =
  Error ("Oh bananas! " ^ msg)

let interpret ast =
  let open Ast in
  let open Core.Result.Monad_infix in
  let eval_literal l: value =
    match l with
    | Literal.Number i -> Value.Number i
    | Literal.Boolean b -> Value.Boolean b
  in

  let rec eval_expression e env heap: (value * env * heap, string) result =
    match e with
    | Expression.Literal l -> Ok (eval_literal l, env, heap)
    | Expression.Binary (lhs, op, rhs) -> begin
      eval_expression lhs env heap >>= fun (lhs, env, heap) ->
      eval_expression rhs env heap >>= fun (rhs, env, heap) ->
      match (lhs, rhs) with
      | (Value.Number lhs, Value.Number rhs) -> begin
        let open Binary_operator in
        match op with
        | Plus -> Ok (Value.Number (lhs + rhs), env, heap)
        | Minus -> Ok (Value.Number (lhs - rhs), env, heap)
        | Times -> Ok (Value.Number (lhs * rhs), env, heap)
        | Divide ->
          if rhs = 0 then
            error "Cannot divide by zero"
          else
            Ok (Value.Number (lhs / rhs), env, heap)
        | Equal -> Ok (Value.Boolean (lhs = rhs), env, heap)
        | Less_than -> Ok (Value.Boolean (lhs < rhs), env, heap)
      end
      | _ -> error "Invalid arithmetic operation" (* TODO: better error message *)
    end
    | Expression.Logical (lhs, op, rhs) -> begin
      eval_expression lhs env heap >>= fun (lhs, env, heap) ->
      let open Logical_operator in
      match lhs, op with
      (* short-circuit boolean operators *)
      | Value.Boolean false, And
      | Value.Boolean true, Or -> Ok (lhs, env, heap)
      (* evaluate second argument*)
      | Value.Boolean true, And
      | Value.Boolean false, Or -> begin
        eval_expression rhs env heap >>= fun (rhs, env, heap) ->
        match rhs with 
        | Value.Boolean _ -> Ok (rhs, env, heap)
        | _ -> error "Invalid logical operation" (* TODO: better error message *)
      end
      (* invalid operators *)
      | _ -> error "Invalid logical operation" (* TODO: better error message *)
    end
    | Expression.Unary (op, rhs) -> begin
      eval_expression rhs env heap >>= fun (rhs, env, heap) ->
      let open Unary_operator in
      match (op, rhs) with
      | (Positive, Value.Number rhs) -> Ok (Value.Number (-rhs), env, heap)
      | (Negate_number, Value.Number rhs) -> Ok (Value.Number (rhs), env, heap)
      | (Negate_bool, Value.Boolean rhs) -> Ok (Value.Boolean (not rhs), env, heap)
      | _ -> error "Invalid unary operation" (* TODO: better error message *)
      end
    | Expression.Conditional (cond, if_t, if_f) -> begin
      eval_expression cond env heap >>= fun (cond, env, heap) ->
      match cond with
      | Value.Boolean cond ->
        if cond then
          eval_expression if_t env heap
        else
          eval_expression if_f env heap
      | _ -> error "Invalid condition in conditional"
    end
    | Expression.Identifier ident ->
      let value = Env.get ~ident ~heap env in
      let ident_error = error "Undefined identifier" in
      Option.value_map value ~default:ident_error ~f:(
        fun value -> Ok (value, env, heap)
      )
    | Expression.Call (fn, arg) -> begin
      eval_expression fn env heap >>= fun (fn, env, heap) ->
      eval_expression arg env heap >>= fun (arg, env, heap) ->
      eval_function_call fn arg heap >>= fun (result, heap) ->
      Ok (result, env, heap)
    end
    | Expression.Function (arg_name, body) ->
      let function_value = Value.Function (arg_name, body, Env.copy env) in
      Ok (function_value, env, heap)
    | Expression.Assignment (ident, expression) ->
      eval_expression expression env heap >>= fun (value, env, heap) ->
      let heap = Env.update ~ident ~value ~heap env in
      match heap with
      | None -> error "Undefined identifier"
      | Some heap ->
        Ok (Value.Void, env, heap)

  and eval_statement s env heap: (value option * env * heap, string) result =
    match s with
    | Statement.Expression_statement e ->
      eval_expression e env heap >>| fun (value, env, heap) ->
      (Some value, env, heap)
    | Statement.Variable_declaration decls ->
      let state = List.fold decls ~init:(Ok (env, heap)) ~f:(fun state (ident, expr) ->
        state >>= fun (env, heap) ->
        eval_expression expr env heap >>= fun (value, env, heap) ->
        let env, heap = Env.set ~ident ~heap ~value env in
        Ok (env, heap)
      ) in
      state >>| fun (env, heap) ->
      (None, env, heap)
    | Statement.Block_statement stmts ->
      eval_statement_list stmts env heap >>| fun (_results, env, heap) ->
      (* a block doesn't return its results *)
      (None, env, heap)
    | Statement.Return_statement _expression ->
      error "Return statement outside of a function"
    
  and eval_function_call fn arg heap: (value * heap, string) result =
    match fn with
    | Value.Function (arg_name, body, env) ->
      let env, heap =
        Env.with_parent env
        |> Env.set ~ident:arg_name ~value:arg ~heap
      in
      eval_function_body body env heap
    | _ -> error "Non-function item cannot be called"
  
  and eval_function_body stmts env heap: (value * heap, string) result =
    match stmts with
    | [] -> Ok (Value.Void, heap)
    | (Statement.Return_statement return_expr) :: _rest ->
      eval_expression return_expr env heap >>= fun (return_value, _env, _heap) ->
        Ok (return_value, heap)
    | stmt :: stmts_rest ->
      eval_statement stmt env heap >>= fun (_output, env, heap) ->
      eval_function_body stmts_rest env heap
  
  and eval_statement_list stmts env heap: (value list * env * heap, string) result =
    List.fold stmts ~init:(Ok ([], env, heap)) ~f:(fun prev_result s ->
      prev_result >>= fun (prev_outputs, env, heap) ->
      eval_statement s env heap >>= fun (output, env, heap) ->
      let output = Option.to_list output in
      Ok (prev_outputs @ output, env, heap)
    )
  in
  
  let eval_program p env heap: (value list * env * heap, string) result =
    match p with
    | Program.Program stmts ->
      eval_statement_list stmts env heap
  in
  
  eval_program ast (Env.empty ()) (Heap.empty ())

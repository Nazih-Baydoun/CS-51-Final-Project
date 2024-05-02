(* Evaluation Functions File - eval.ml *)
open Types

(* Lexically Scoped Evaluation Function *)
let rec eval_s env expr =
  match expr with
  | Num n -> IntVal n
  | Bool b -> BoolVal b
  | Var x ->
      (try List.assoc x env
       with Not_found -> failwith ("Unbound variable: " ^ x))
  | Unop (Negate, e) ->
      begin match eval_s env e with
      | IntVal n -> IntVal (-n)
      | _ -> failwith "Negation applied to non-integer"
      end
  | Binop (op, e1, e2) ->
      let v1 = eval_s env e1 in
      let v2 = eval_s env e2 in
      begin match (v1, v2, op) with
        | (IntVal v1, IntVal v2, Plus) -> IntVal (v1 + v2)
        | (IntVal v1, IntVal v2, Minus) -> IntVal (v1 - v2)
        | (IntVal v1, IntVal v2, Times) -> IntVal (v1 * v2)
        | (IntVal v1, IntVal v2, Equals) -> BoolVal (v1 = v2)
        | (IntVal v1, IntVal v2, LessThan) -> BoolVal (v1 < v2)
        | _ -> failwith "Invalid types for binary operation"
      end
  | Conditional (e1, e2, e3) ->
      begin match eval_s env e1 with
        | BoolVal true -> eval_s env e2
        | BoolVal false -> eval_s env e3
        | _ -> failwith "Expected boolean in conditional"
      end
  | Fun (x, e) -> FunVal (x, e, ref env)
  | App (e1, e2) ->
      let closure = eval_s env e1 in
      let arg = eval_s env e2 in
      begin match closure with
        | FunVal (x, body, f_env) -> eval_s ((x, arg) :: !f_env) body
        | _ -> failwith "Expected function in application"
      end
  | Let (x, e1, e2) ->
      let v1 = eval_s env e1 in
      eval_s ((x, v1) :: env) e2
  | Letrec (x, e1, e2) ->
      let rec_env = ref env in
      let v1 = FunVal (x, e1, rec_env) in
      rec_env := (x, v1) :: env;
      eval_s !rec_env e2
  | List exprs -> ListVal (List.map (eval_s env) exprs)
  | Head expr ->
      begin match eval_s env expr with
        | ListVal (h::_) -> h
        | ListVal [] -> failwith "Head called on empty list"
        | _ -> failwith "Head called on non-list"
      end
  | Tail expr ->
      begin match eval_s env expr with
        | ListVal (_::t) -> ListVal t
        | ListVal [] -> failwith "Tail called on empty list"
        | _ -> failwith "Tail called on non-list"
      end
  | EmptyList -> ListVal []
  | Raise -> failwith "Exception raised"
  | Unassigned -> failwith "Unassigned variable access"

(* Dynamically Scoped Evaluation Function *)
let rec eval_d env expr =
  match expr with
  | Num n -> IntVal n
  | Bool b -> BoolVal b
  | Var x ->
      (try List.assoc x env
       with Not_found -> failwith ("Unbound variable: " ^ x))
  | Unop (Negate, e) ->
      begin match eval_d env e with
      | IntVal n -> IntVal (-n)
      | _ -> failwith "Negation applied to non-integer"
      end
  | Binop (op, e1, e2) ->
      let v1 = eval_d env e1 in
      let v2 = eval_d env e2 in
      begin match (v1, v2, op) with
        | (IntVal v1, IntVal v2, Plus) -> IntVal (v1 + v2)
        | (IntVal v1, IntVal v2, Minus) -> IntVal (v1 - v2)
        | (IntVal v1, IntVal v2, Times) -> IntVal (v1 * v2)
        | (IntVal v1, IntVal v2, Equals) -> BoolVal (v1 = v2)
        | (IntVal v1, IntVal v2, LessThan) -> BoolVal (v1 < v2)
        | _ -> failwith "Invalid types for binary operation"
      end
  | Conditional (e1, e2, e3) ->
      begin match eval_d env e1 with
        | BoolVal true -> eval_d env e2
        | BoolVal false -> eval_d env e3
        | _ -> failwith "Expected boolean in conditional"
      end
  | Fun (x, e) -> Closure (x, e, ref env)
  | App (e1, e2) ->
      let closure = eval_d env e1 in
      let arg = eval_d env e2 in
      begin match closure with
        | Closure (x, body, f_env) -> eval_d ((x, arg) :: !f_env) body
        | _ -> failwith "Expected function in application"
      end
  | Let (x, e1, e2) ->
      let v1 = eval_d env e1 in
      eval_d ((x, v1) :: env) e2
  | Letrec (x, e1, e2) ->
    let rec_env = ref env in
    let v1 = FunVal (x, e1, rec_env) in
    rec_env := (x, v1) :: env;
    eval_s !rec_env e2
  | List exprs -> ListVal (List.map (eval_d env) exprs)
  | Head expr ->
      begin match eval_d env expr with
        | ListVal (h::_) -> h
        | ListVal [] -> failwith "Head called on empty list"
        | _ -> failwith "Head called on non-list"
      end
  | Tail expr ->
      begin match eval_d env expr with
        | ListVal (_::t) -> ListVal t
        | ListVal [] -> failwith "Tail called on empty list"
        | _ -> failwith "Tail called on non-list"
      end
  | EmptyList -> ListVal []
  | Raise -> failwith "Exception raised"
  | Unassigned -> failwith "Unassigned variable access"

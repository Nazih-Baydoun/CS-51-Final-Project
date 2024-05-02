(*
                         CS 51 Final Project
                           MiniML -- Evaluation
*)

open Expr ;;
  
exception EvalError of string ;;
exception EvalException ;;

module type Env_type = sig
  type env
  type value = 
    | Val of Expr.expr
    | Closure of Expr.expr * env  
  val empty : env
  val lookup : env -> Expr.varid -> value
  val extend : env -> Expr.varid -> value -> env
  val env_to_string : env -> string
  val value_to_string : value -> string  
end

module Env : Env_type = struct

  type env = (Expr.varid * value) list
   and value = 
     | Val of Expr.expr
     | Closure of Expr.expr * env

  let empty = []
  
  let rec lookup env v =
    match env with
    | [] -> raise (EvalError ("Unbound variable: " ^ v))
    | (var,value)::t -> if var = v then value else lookup t v

  let extend env v value = (v,value)::env

  let rec env_to_string env =
    match env with
    | [] -> ""  
    | (var,value)::t -> 
        var ^ "=" ^ value_to_string value ^ "\n" ^ env_to_string t

  and value_to_string value =
    match value with
    | Val e -> Expr.exp_to_abstract_string e
    | Closure (e,env) -> "<closure: " ^ Expr.exp_to_abstract_string e ^ 
                         " ; environment>" ^ env_to_string env

end

let binop_eval op v1 v2 = 
  match v1, v2 with
  | Env.Val (Expr.Num x), Env.Val (Expr.Num y) -> 
    (match op with
     | Expr.Plus -> Env.Val (Expr.Num (x + y)) 
     | Expr.Minus -> Env.Val (Expr.Num (x - y))
     | Expr.Times -> Env.Val (Expr.Num (x * y))
     | Expr.Equals -> Env.Val (Expr.Bool (x = y)) 
     | Expr.LessThan -> Env.Val (Expr.Bool (x < y)))
  | Env.Val (Expr.Bool x), Env.Val (Expr.Bool y) ->
    (match op with  
     | Expr.Equals -> Env.Val (Expr.Bool (x = y))
     | _ -> raise (EvalError "Invalid operator for bools"))
  | _ -> raise (EvalError "Operator applied to invalid types")
  

let unop_eval op v =
  match v with 
  | Env.Val (Expr.Num x) ->
    (match op with
     | Expr.Negate -> Env.Val (Expr.Num (-x))) 
  | _ -> raise (EvalError "Operator applied to invalid type")


let rec eval_s (e:Expr.expr) : Expr.expr =
  match e with
  | Expr.Var _ -> raise (EvalError "Free variable")
  | Expr.Num _ -> e
  | Expr.Bool _ -> e  
  | Expr.Binop (op, e1, e2) ->
    let v1 = Env.Val (eval_s e1) in
    let v2 = Env.Val (eval_s e2) in
    (match binop_eval op v1 v2 with
     | Env.Val result -> result  
     | _ -> raise (EvalError "binop_eval returned non-value"))
  | Expr.Unop (op, e) ->
    let v = Env.Val (eval_s e) in
    (match unop_eval op v with
     | Env.Val result -> result
     | _ -> raise (EvalError "unop_eval returned non-value")) 
  | Expr.Conditional (e1,e2,e3) -> 
    (match eval_s e1 with
     | Expr.Bool b -> if b then eval_s e2 else eval_s e3
     | _ -> raise (EvalError "Non-boolean condition"))
  | Expr.Fun (v,e) -> Expr.Fun(v,e)
  | Expr.Let (v,def,body) ->
    eval_s (Expr.subst (Expr.Var v) (eval_s def) body)
  | Expr.Letrec (v,def,body) ->
    let arg = Expr.Var v in
    eval_s (Expr.subst v (Expr.Letrec (v,def, arg)) body) 
  | Expr.App (e1, e2) ->
    (match eval_s e1 with
     | Expr.Fun (x,e) -> eval_s (Expr.subst x (eval_s e2) e)   
     | _ -> raise (EvalError "Non-function in application"))
  | Expr.Raise -> raise EvalException
  | Expr.Unassigned -> raise (EvalError "Unassigned value")
    
  
let rec eval_d (e:Expr.expr) (env:Env.env) : Env.value =
  match e with
  | Expr.Var x -> Env.lookup env x
  | Expr.Num n -> Env.Val e
  | Expr.Bool b -> Env.Val e
  | Expr.Binop (op, e1, e2) ->
    binop_eval op (eval_d e1 env) (eval_d e2 env)
  | Expr.Unop (op, e) -> unop_eval op (eval_d e env)
  | Expr.Conditional (e1,e2,e3) -> 
    (match eval_d e1 env with
     | Env.Val (Expr.Bool b) -> if b then eval_d e2 env else eval_d e3 env
     | _ -> raise (EvalError "Non-boolean condition"))
  | Expr.Fun (x,e) -> Env.Closure(Expr.Fun(x,e), env)
  | Expr.Let (v,def,body) ->
    let vdef = eval_d def env in
    let new_env = Env.extend env v vdef in
    eval_d body new_env
  | Expr.Letrec (v,def,body) ->
    let dummy = Env.Val Expr.Unassigned in
    let new_env = Env.extend env v dummy in
    let vdef = eval_d def new_env in
    let newer_env = Env.extend (Env.empty) v vdef in 
    eval_d body newer_env      
  | Expr.App (e1,e2) -> 
    (match eval_d e1 env with
     | Env.Closure (Expr.Fun (x,e), defenv) ->  
       let v2 = eval_d e2 env in
       let nenv = Env.extend defenv x v2 in
       eval_d e nenv
     | _ -> raise (EvalError "Non-function in application"))
  | Expr.Raise -> raise EvalException  
  | Expr.Unassigned -> raise (EvalError "Unassigned value") 

    
let rec eval_l (e:Expr.expr) (env:Env.env) : Env.value =
  match e with
  | Expr.Var x -> Env.lookup env x
  | Expr.Num n -> Env.Val e
  | Expr.Bool b -> Env.Val e  
  | Expr.Binop (op, e1, e2) ->
    binop_eval op (eval_l e1 env) (eval_l e2 env)
  | Expr.Unop (op, e) -> unop_eval op (eval_l e env)
  | Expr.Conditional (e1,e2,e3) -> 
    (match eval_l e1 env with
     | Env.Val (Expr.Bool b) -> if b then eval_l e2 env else eval_l e3 env
     | _ -> raise (EvalError "Non-boolean condition"))
  | Expr.Fun (x,e) -> Env.Closure (e, env)  
  | Expr.Let (v,def,body) ->
    let vdef = eval_l def env in
    let new_env = Env.extend env v vdef in 
    eval_l body new_env
  | Expr.Letrec (v,def,body) ->
    let dummy = Env.Val Expr.Unassigned in
    let new_env = Env.extend env v dummy in
    let vdef = eval_l def new_env in
    (match vdef with
     | Env.Val (Expr.Fun _) -> 
       let newer_env = Env.extend new_env v vdef in
       eval_l body newer_env
     | _ -> raise (EvalError "Letrec must define function"))
  | Expr.App (e1,e2) ->
    (match eval_l e1 env with 
     | Env.Closure(Expr.Fun (x,body),defenv) ->
       let v2 = eval_l e2 env in
       eval_l body (Env.extend defenv x v2)
     | _ -> raise (EvalError "Non-function in application"))
  | Expr.Raise -> raise EvalException
  | Expr.Unassigned -> raise (EvalError "Unassigned value")
       

(* evaluate : Expr.expr -> Expr.expr *)
let evaluate e = 
  eval_l e Env.empty
  |> fun v ->   
     (match v with
      | Env.Val e -> e  
      | _ -> raise (EvalError "evaluation returned non-value"))
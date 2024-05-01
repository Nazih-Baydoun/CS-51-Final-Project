(* unary and binary operators defs *)
type unop = Negate

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan

(* expression types *)
type varid = string

type expr =
  | Var of varid
  | Num of int
  | Bool of bool
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Conditional of expr * expr * expr
  | Fun of varid * expr
  | Let of varid * expr * expr
  | Letrec of varid * expr * expr
  | App of expr * expr
  | List of expr list  
  | Head of expr      
  | Tail of expr       
  | EmptyList   
  | Raise
  | Unassigned

and value =
  | IntVal of int
  | BoolVal of bool
  | FunVal of varid * expr * env
  | ListVal of value list  
  | Closure of varid * expr * env ref
  | Unassigned

and env = (varid * value) list

(* exception for evaluation errors *)
exception Eval_error of string

(* fun to make expressions -> string *)
let rec exp_to_string expr =
  match expr with
  | Var v -> v
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unop (Negate, e) -> "(-" ^ exp_to_string e ^ ")"
  | Binop (op, e1, e2) ->
    let op_str = match op with
      | Plus -> "+"
      | Minus -> "-"
      | Times -> "*"
      | Equals -> "="
      | LessThan -> "<"
    in
    "(" ^ exp_to_string e1 ^ " " ^ op_str ^ " " ^ exp_to_string e2 ^ ")"
  | Conditional (e1, e2, e3) ->
    "if " ^ exp_to_string e1 ^ " then " ^ exp_to_string e2 ^ " else " ^ exp_to_string e3
  | Fun (x, e) -> "fun " ^ x ^ " -> " ^ exp_to_string e
  | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^ exp_to_string e2
  | Letrec (x, e1, e2) -> "let rec " ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^ exp_to_string e2
  | App (e1, e2) -> exp_to_string e1 ^ " (" ^ exp_to_string e2 ^ ")"
  | List exprs -> "[" ^ String.concat "; " (List.map exp_to_string exprs) ^ "]"
  | Head expr -> "head(" ^ exp_to_string expr ^ ")"
  | Tail expr -> "tail(" ^ exp_to_string expr ^ ")"
  | EmptyList -> "[]"
  | Raise -> "raise"
  | Unassigned -> "unassigned"

(* lexically scoped func *)
let rec eval_s env expr = 
  match expr with
  | Num n -> IntVal n
  | Bool b -> BoolVal b
  | Var x -> 
      (try List.assoc x env 
       with Not_found -> failwith ("Unbound variable: " ^ x))
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
  | Fun (x, e) ->
        print_endline ("Creating function: " ^ x);
        FunVal (x, e, env)
    
  | App (e1, e2) ->
    let closure = eval_s env e1 in
    let arg = eval_s env e2 in
    begin match closure with
      | FunVal (x, body, f_env) ->
        print_endline ("Applying function " ^ x ^ " with arg: " ^ 
                       (match arg with IntVal n -> string_of_int n | _ -> "non-int"));
        let result = eval_s ((x, arg) :: f_env) body in
        print_endline ("Result of application: " ^ (match result with IntVal n -> string_of_int n | _ -> "non-int"));
        result
      | _ -> failwith ("Expected function, got: " ^ exp_to_string e1)
    end

    
  | Let (x, e1, e2) ->
      let v1 = eval_s env e1 in
      eval_s ((x, v1) :: env) e2
  | Letrec (x, e1, e2) ->
      let rec bind fenv = 
          (x, FunVal (x, e1, fenv)) :: fenv  (* Ensure x is bound to a function that carries fenv *)
        in
        let new_env = bind env in  (* new_env should now carry the recursive binding of x *)
        eval_s new_env e2
    
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


(* dynamicaly scoped func *)
let rec eval_d env expr =
  match expr with
  | Num n -> IntVal n
  | Bool b -> BoolVal b
  | Var x -> 
      (try List.assoc x env
       with Not_found -> failwith ("Unbound variable: " ^ x))
  | Binop (op, e1, e2) ->
      let IntVal v1 = eval_d env e1 in
      let IntVal v2 = eval_d env e2 in
      (match op with
       | Plus -> IntVal (v1 + v2)
       | Minus -> IntVal (v1 - v2)
       | Times -> IntVal (v1 * v2)
       | Equals -> BoolVal (v1 = v2)
       | LessThan -> BoolVal (v1 < v2))
  | Conditional (e1, e2, e3) ->
      let BoolVal cond = eval_d env e1 in
      if cond then eval_d env e2 else eval_d env e3
  | Fun (x, e) -> Closure (x, e, ref env)  
  | App (e1, e2) ->
      let Closure (x, body, closure_env) = eval_d env e1 in
      let arg = eval_d env e2 in
      eval_d ((x, arg) :: !closure_env) body  
  | Let (x, e1, e2) ->
      let v1 = eval_d env e1 in
      eval_d ((x, v1) :: env) e2
  | Letrec (x, e1, e2) ->
      let rec bind fenv = (x, Closure (x, e1, ref fenv)) :: fenv in 
      eval_d (bind env) e2
  | Raise -> failwith "Exception raised"
  | Unassigned -> failwith "Unassigned variable access"

  let test_expressions = [
    ("Num 5", Num 5);
    ("Addition 3 + 7", Binop (Plus, Num 3, Num 7));
    ("Variable x", Var "x");
    ("Conditional true then 42 else 0", Conditional (Bool true, Num 42, Num 0));
    ("Undefined var u", Var "u");
    ("List creation", List [Num 1; Num 2; Num 3]);
    ("List head", Head (Var "z"));
    ("List tail", Tail (Var "z"));
    ("Empty list head", Head EmptyList);
    ("Nested function", Let ("f", Fun ("a", Fun ("b", Binop (Plus, Var "a", Var "b"))), App (App (Var "f", Num 5), Num 7)));
    ("Recursive factorial", Letrec ("fact", Fun ("n", Conditional (Binop (Equals, Var "n", Num 0), Num 1, Binop (Times, Var "n", App (Var "fact", Binop (Minus, Var "n", Num 1))))), App (Var "fact", Num 5)));
    ("List inside conditional", Conditional (Bool true, List [Num 10; Num 20], EmptyList));
    ("Complex list operation", Tail (List [Num 10; Num 20; Num 30]));
]

let () =
  let test_env = [("x", IntVal 10); ("y", BoolVal true); ("z", ListVal [IntVal 1; IntVal 2; IntVal 3])] in
  print_endline "Starting test cases:";
  List.iter (fun (desc, expr) ->
    try
      let result = eval_s test_env expr in
      print_endline (desc ^ ": " ^ match result with
        | IntVal n -> string_of_int n
        | BoolVal b -> string_of_bool b
        | ListVal l -> "[" ^ String.concat "; " (List.map (function IntVal i -> string_of_int i | _ -> "") l) ^ "]"
        | _ -> "Invalid result type")
    with
    | Failure msg -> print_endline (desc ^ ": " ^ "Error - " ^ msg)
    | Eval_error msg -> print_endline (desc ^ ": " ^ "Evaluation error - " ^ msg)
    | _ -> print_endline (desc ^ ": Unhandled exception")
  ) test_expressions

let () =
  let factorial_test = 
    Letrec ("fact", 
      Fun ("n", Conditional (Binop (Equals, Var "n", Num 0),
                              Num 1,
                              Binop (Times, Var "n", App (Var "fact", Binop (Minus, Var "n", Num 1))))),
      App (Var "fact", Num 5))
  in
  let result = eval_s [] factorial_test in
  match result with
  | IntVal n -> print_endline ("Factorial of 5 is " ^ string_of_int n)
  | _ -> print_endline "Factorial test failed with invalid result type"

  
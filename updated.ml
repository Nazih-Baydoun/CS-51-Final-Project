(* Definition of unary and binary operators *)
type unop = Negate

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan

(* Types for variable identifiers and expressions *)
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
  | Raise
  | Unassigned

(* Simultaneous definition of value and env types using mutual recursion *)
and value =
  | IntVal of int
  | BoolVal of bool
  | FunVal of varid * expr * env
  | Closure of varid * expr * env ref  (* Keep if your design uses Closure differently *)
  | Unassigned

and env = (varid * value) list

(* Exception for evaluation errors *)
exception Eval_error of string

(* Function to convert expressions to string *)
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
  | Raise -> "raise"
  | Unassigned -> "unassigned"

let () =
  (* Define a few test expressions *)
  let expressions = [
    Var "x";
    Num 42;
    Bool true;
    Binop (Plus, Num 1, Num 2);
    Conditional (Bool true, Num 1, Num 0);
    Fun ("x", Binop (Plus, Var "x", Num 1));
    Let ("x", Num 5, Binop (Plus, Var "x", Num 3));
    App (Fun ("x", Binop (Plus, Var "x", Num 1)), Num 5);
    Letrec ("fact", 
            Fun ("n", Conditional (Binop (Equals, Var "n", Num 0), 
                                    Num 1, 
                                    Binop (Times, Var "n", App (Var "fact", Binop (Minus, Var "n", Num 1))))),
            App (Var "fact", Num 5))
  ] in

  (* Convert each expression to a string and print it *)
  List.iter (fun expr ->
    print_endline (exp_to_string expr)
  ) expressions
  (* No need for anything after the iter call if this is the end of the block *)

(* Evaluation function *)
let rec eval_s env expr = 
    match expr with
    | Num n -> IntVal n
    | Bool b -> BoolVal b
    | Var x -> 
        (try List.assoc x env 
         with Not_found -> failwith ("Unbound variable: " ^ x))
    | Binop (op, e1, e2) ->
        let IntVal v1 = eval_s env e1 in
        let IntVal v2 = eval_s env e2 in
        (match op with
         | Plus -> IntVal (v1 + v2)
         | Minus -> IntVal (v1 - v2)
         | Times -> IntVal (v1 * v2)
         | Equals -> BoolVal (v1 = v2)
         | LessThan -> BoolVal (v1 < v2))
    | Conditional (e1, e2, e3) ->
        let BoolVal cond = eval_s env e1 in
        if cond then eval_s env e2 else eval_s env e3
    | Fun (x, e) -> FunVal (x, e, env)
    | App (e1, e2) ->
        let FunVal (x, body, f_env) = eval_s env e1 in
        let arg = eval_s env e2 in
        eval_s ((x, arg) :: f_env) body
    | Let (x, e1, e2) ->
        let v1 = eval_s env e1 in
        eval_s ((x, v1) :: env) e2
    | Letrec (x, e1, e2) ->
        let rec bind fenv = (x, FunVal (x, e1, fenv)) :: fenv
        in eval_s (bind env) e2

(* Test expressions and their output *)
let () =
  let expressions = [
    Var "x";
    Num 42;
    Bool true;
    Binop (Plus, Num 1, Num 2);
    Conditional (Bool true, Num 1, Num 0);
    Fun ("x", Binop (Plus, Var "x", Num 1));
    Let ("x", Num 5, Binop (Plus, Var "x", Num 3));
    App (Fun ("x", Binop (Plus, Var "x", Num 1)), Num 5);
    Letrec ("fact", 
            Fun ("n", Conditional (Binop (Equals, Var "n", Num 0), 
                                    Num 1, 
                                    Binop (Times, Var "n", App (Var "fact", Binop (Minus, Var "n", Num 1))))),
            App (Var "fact", Num 5))
  ] in
  List.iter (fun expr ->
    print_endline (exp_to_string expr)
  ) expressions

(* Tests for evaluation function *)

(* Tests for evaluation function *)
let () =
  let test_env = [("x", IntVal 10); ("y", BoolVal true)] in
  let test_expressions = [
    ("Num 5", Num 5);
    ("Addition 3 + 7", Binop (Plus, Num 3, Num 7));
    ("Variable x", Var "x");
    ("Conditional true then 42 else 0", Conditional (Bool true, Num 42, Num 0));
    ("Undefined var z", Var "z")
  ] in
  List.iter (fun (desc, expr) ->
    try
      let result = eval_s test_env expr in
      print_endline (desc ^ ": " ^ match result with
        | IntVal n -> string_of_int n
        | BoolVal b -> string_of_bool b
        | _ -> "Invalid result type")
    with
    | Failure msg -> print_endline (desc ^ ": " ^ "Error - " ^ msg)
    | Eval_error msg -> print_endline (desc ^ ": " ^ "Evaluation error - " ^ msg)
    | _ -> print_endline (desc ^ ": Unhandled exception")
  ) test_expressions



  let () =
  print_endline "\nTesting dynamically scoped evaluation:";
  let dynamic_env = [("x", IntVal 2)] in
  let test_expressions = [
    ("Direct application", App (Fun ("x", Var "x"), Num 5));  (* Should return 5 in dynamic scoping *)
    ("Enclosed application", Let ("x", Num 10, App (Fun ("y", Var "x"), Num 5)))  (* Should return 10 in dynamic scoping *)
  ] in
  List.iter (fun (desc, expr) ->
    try
      let result = eval_d dynamic_env expr in
      print_endline (desc ^ ": " ^ match result with
        | IntVal n -> string_of_int n
        | BoolVal b -> string_of_bool b
        | _ -> "Invalid result type")
    with
    | Failure msg -> print_endline (desc ^ ": " ^ "Error - " ^ msg)
    | Eval_error msg -> print_endline (desc ^ ": " ^ "Evaluation error - " ^ msg)
    | _ -> print_endline (desc ^ ": Unhandled exception")
  ) test_expressions
(* Unary and binary operator definitions *)
type unop = Negate

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan

(* Expression types *)
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
  | FunVal of varid * expr * env ref
  | ListVal of value list
  | Closure of varid * expr * env ref
  | Unassigned

and env = (varid * value) list

(* Exception for evaluation errors *)
exception Eval_error of string

(* Convert expressions to strings *)
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

(* Test cases to ensure correctness *)
(* Test cases to ensure correctness *)
let () =
  let test_env = [("x", IntVal 10); ("y", BoolVal true); ("z", ListVal [IntVal 1; IntVal 2; IntVal 3])] in
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
  ] in
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


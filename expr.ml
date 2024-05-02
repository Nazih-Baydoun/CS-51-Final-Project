(* Expression Utilities File - expr.ml *)
open Types

(* Function to identify free variables in an expression *)
let rec free_vars expr = match expr with
  | Var x -> [x]
  | Num _ | Bool _ | EmptyList | Raise | Unassigned -> []
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) | App (e1, e2) -> List.append (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> List.concat [free_vars e1; free_vars e2; free_vars e3]
  | Fun (x, e) -> List.filter (fun v -> v <> x) (free_vars e)
  | Let (x, e1, e2) | Letrec (x, e1, e2) -> List.append (free_vars e1) (List.filter (fun v -> v <> x) (free_vars e2))
  | List es -> List.concat (List.map free_vars es)
  | Head e | Tail e -> free_vars e

(* Convert expressions to strings for testing and output purposes *)
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

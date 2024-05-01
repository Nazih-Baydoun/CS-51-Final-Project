type unop = Negate

type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan

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

type env = (varid * value) list  

and value =  
  | Int of int
  | Bool of bool
  | Closure of varid * expr * env ref
  | Unassigned

exception Eval_error of string


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
  | Letrec (x, e1, e2) -> 
    "let rec " ^ x ^ " = " ^ exp_to_string e1 ^ " in " ^ exp_to_string e2
  | App (e1, e2) -> exp_to_string e1 ^ " " ^ exp_to_string e2

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

let rec free_vars expr =
  match expr with
  | Var x -> [x]  (* A variable on its own is free *)
  | Num _ | Bool _ | Raise | Unassigned -> []  (* Literals and special values don't have free variables *)
  | Unop (_, e) -> free_vars e  (* Unary operations have the same free variables as their operand *)
  | Binop (_, e1, e2) | App (e1, e2) -> 
      List.append (free_vars e1) (free_vars e2)  (* Combine the free variables from both subexpressions *)
  | Conditional (e1, e2, e3) -> 
      List.concat [free_vars e1; free_vars e2; free_vars e3]  (* Combine from all three subexpressions *)
  | Fun (x, e) | Let (x, _, e) -> 
      List.filter (fun v -> v <> x) (free_vars e)  (* Exclude the bound variable from the free variables of the body *)
  | Letrec (x, e1, e2) -> 
      let e1_vars = List.filter (fun v -> v <> x) (free_vars e1) in
      let e2_vars = List.filter (fun v -> v <> x) (free_vars e2) in
      List.append e1_vars e2_vars  (* Exclude the bound variable from both, as it might be bound in either *)

(* Utility to remove duplicates from a list of variables, can be used if needed *)
let rec remove_duplicates lst =
  match lst with
  | [] -> []
  | h :: t -> h :: remove_duplicates (List.filter (fun x -> x <> h) t)

(* Example usage within the file *)
(* Define expressions to test for free variables *)
let test_free_vars () =
  let expr1 = Binop (Plus, Var "x", Num 10) in
  let expr2 = Fun ("x", Binop (Plus, Var "x", Num 10)) in
  let expr3 = Let ("x", Num 10, Binop (Plus, Var "x", Var "y")) in
  let expr4 = Letrec ("fact", 
                      Fun ("n", Conditional (Binop (Equals, Var "n", Num 0), 
                                              Num 1, 
                                              Binop (Times, Var "n", App (Var "fact", Binop (Minus, Var "n", Num 1))))),
                      App (Var "fact", Var "x")) in

  print_endline ("Free variables in expr1 (x + 10): " ^ String.concat ", " (free_vars expr1));
  print_endline ("Free variables in expr2 (fun x -> x + 10): " ^ String.concat ", " (free_vars expr2));
  print_endline ("Free variables in expr3 (let x = 10 in x + y): " ^ String.concat ", " (free_vars expr3));
  print_endline ("Free variables in expr4 (let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact x): " ^ String.concat ", " (free_vars expr4))

(* Entry point to run all tests *)
let () =
  print_endline "Testing exp_to_string outputs:";
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
  ) expressions;

  print_endline "\nTesting free_vars function:";
  test_free_vars ()

let rec subst expr var new_expr =
    match expr with
    | Var x -> if x = var then new_expr else expr  (* Replace variable with new_expr if it matches var *)
    | Num _ | Bool _ | Raise | Unassigned -> expr  (* Return expression as is for literals and special values *)
    | Unop (op, e) -> Unop (op, subst e var new_expr)  (* Apply substitution to the operand of the unary operator *)
    | Binop (op, e1, e2) -> 
        Binop (op, subst e1 var new_expr, subst e2 var new_expr)  (* Apply substitution to both operands *)
    | Conditional (e1, e2, e3) -> 
        Conditional (subst e1 var new_expr, subst e2 var new_expr, subst e3 var new_expr)  (* Apply to all parts of conditional *)
    | Fun (x, e) -> 
        if x = var then 
          Fun (x, e)  (* Do not substitute inside the function body if it binds the variable *)
        else 
          Fun (x, subst e var new_expr)
    | Let (x, e1, e2) ->
        Let (x, subst e1 var new_expr, if x = var then e2 else subst e2 var new_expr)  (* Substitute in e1 always, in e2 only if x ≠ var *)
    | Letrec (x, e1, e2) ->
        Letrec (x, subst e1 var new_expr, if x = var then e2 else subst e2 var new_expr)  (* Same as Let but handles recursive definitions *)
    | App (e1, e2) -> 
        App (subst e1 var new_expr, subst e2 var new_expr)  (* Apply substitution to both function and argument expressions *)
  


let () =
  let expr = Let ("x", Num 5, Binop (Plus, Var "x", Num 10)) in
  let substituted_expr = subst expr "x" (Num 20) in  
  print_endline (exp_to_string substituted_expr)  

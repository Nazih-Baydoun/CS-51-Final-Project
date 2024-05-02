(* Types Definition File - types.ml *)

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

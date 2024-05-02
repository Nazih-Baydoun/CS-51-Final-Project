
(* The type of tokens. *)

type token = 
  | TRUE
  | TIMES
  | THEN
  | REC
  | RAISE
  | PLUS
  | OPEN
  | NEG
  | MINUS
  | LET
  | LESSTHAN
  | INT of (int)
  | IN
  | IF
  | ID of (string)
  | FUNCTION
  | FALSE
  | EQUALS
  | EOF
  | ELSE
  | DOT
  | CLOSE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val input: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Expr.expr)

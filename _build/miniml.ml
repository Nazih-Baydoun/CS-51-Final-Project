(*
                         CS 51 Final Project
                        MiniML -- Read-Eval-Print Loop
*)

module Parser = Miniml_parse;;
module Lexer = Miniml_lex ;;
module Expr = Expr ;;
module Evaluation = Evaluation ;;

open Printf ;;

(* str_to_exp : string -> Expr.expr 
   Parse a string into an expression, raising Failure if the expression
   is syntactically invalid.
 *)
let str_to_exp (str: string) : Expr.expr =
  let lexbuf = Lexing.from_string str in
  let exp = Parser.input Lexer.token lexbuf in
  exp
;;

(* repl : unit -> unit
   Read-eval-print loop for MiniML.
*)  
let repl () =
  (* lexical analyzer buffer from stdin *)
  let lexbuf = Lexing.from_channel stdin in
  (* set up the initial environment *)
  let env = Evaluation.Env.empty in
  
  printf "Welcome to MiniML. Type on the #> prompt.\n";
  
  (* the main LOOP *)
  while true do
    (try 
       (* prompt *)
       printf "#> ";
       flush stdout;
       
       (* read and parse an expression from the input *)
       let exp = Parser.input Lexer.token lexbuf in
       
       (* evaluate it *)
       let result = Evaluation.evaluate exp in
       
       (* print the result *)
       printf "==> %s\n" (Expr.exp_to_string result);
       flush stdout;
       
     with 
     | Parsing.Parse_error ->
       printf "xx> parse error\n"; flush stdout
     | Evaluation.EvalError msg -> 
       printf "xx> evaluation error: %s\n" msg; flush stdout
     | Evaluation.EvalException ->
       printf "xx> evaluation exception\n"; flush stdout
     | End_of_file -> 
       printf "Goodbye.\n"; 
       exit 0
    );
  done
;;
        
(* Run REPL if called from command line *)

try
  let _ = Str.search_forward (Str.regexp "miniml\\.\\(byte\\|native\\)") (Sys.argv.(0)) 0 in
  repl ()
with Not_found -> () ;;
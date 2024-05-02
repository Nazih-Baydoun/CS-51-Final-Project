type unop = Negate
type binop = Plus | Minus | Times | Equals | LessThan
    
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
  | Raise                               
  | Unassigned                          
  | App of expr * expr                  

(* Sets of variable IDs *)
type varidset 

(* free_vars : expr -> varidset
   Return the set of unbound variables in the expression 
*)
val free_vars : expr -> varidset

(* new_varname : string -> unit -> string   
   Return a new varname starting with the given string
*)
val new_varname : string -> unit -> string

(* subst : expr -> varid -> expr -> expr
   Substitute the given expression for free occurrences of the 
   variable in the expression, avoiding variable capture  
*)
val subst : expr -> varid -> expr -> expr

(* exp_to_string : expr -> string
   Convert an expression to a string in concrete syntax
*)  
val exp_to_string : expr -> string

(* exp_to_abstract_string : expr -> string
   Convert an expression to a string in abstract syntax
*)
val exp_to_abstract_string : expr -> string
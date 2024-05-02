(* main.ml *)

(* Assuming `Eval` module has functions `eval_s` and `eval_d` *)

open Types  (* This imports types and exceptions from types.ml *)

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
      let result = Eval.eval_s test_env expr in
      print_endline (desc ^ ": " ^ match result with
        | IntVal n -> string_of_int n
        | BoolVal b -> string_of_bool b
        | ListVal l -> "[" ^ String.concat "; " (List.map (function IntVal i -> string_of_int i | _ -> "") l) ^ "]"
        | _ -> "Invalid result type")
    with
    | Failure msg -> print_endline (desc ^ ": Error - " ^ msg)
    | Eval_error msg -> print_endline (desc ^ ": Evaluation error - " ^ msg)
    | _ -> print_endline (desc ^ ": Unhandled exception")
  ) test_expressions

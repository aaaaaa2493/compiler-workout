open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let eval_one_instriction config_sm instruction = 
	let (stack, config) = config_sm in
	let (st, input, output) = config in

	match instruction with
	| BINOP op -> (match stack with
		              | y::x::rest -> [Language.Expr.calc op x y] @ rest, config
                )

  | CONST x  -> [x] @ stack, config

	| READ     -> (match input with
		              | x::rest -> [x] @ stack, (st, rest, output)
                )

	| WRITE    -> (match stack with
		              | x::rest -> rest, (st, input, output @ [x])
                )

	| LD var   -> [st var] @ stack, config

	| ST var   -> (match stack with
		              | x::rest -> rest, (Language.Expr.update var x st, input, output)
                )

let eval config_sm prog = List.fold_left eval_one_instriction config_sm prog

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile_expr (expr: Language.Expr.t) = match expr with
  | Language.Expr.Const x                 -> [CONST x]
  | Language.Expr.Var x                   -> [LD x]
  | Language.Expr.Binop (op, left, right) -> (compile_expr left) @ (compile_expr right) @ [BINOP op]

let rec compile (program: Language.Stmt.t) = match program with
  | Language.Stmt.Read var                -> [READ; ST var]
  | Language.Stmt.Write expr              -> (compile_expr expr) @ [WRITE]
  | Language.Stmt.Assign (var, expr)      -> (compile_expr expr) @ [ST var]
  | Language.Stmt.Seq (curr, next)        -> (compile curr) @ (compile next)

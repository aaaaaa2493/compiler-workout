open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env ((stack, ((state, input, output) as c)) as config) = function
| [] -> config
| instruction :: rest_instr ->
     match instruction with
     | BINOP op ->
          ( match stack with
          | y::x::rest -> eval env ((Expr.calc op x y) :: rest, c) rest_instr )

     | CONST v -> eval env (v::stack, c) rest_instr

     | READ ->
        ( match input with
          | x::rest -> eval env (x::stack, (state, rest, output)) rest_instr )

     | WRITE ->
        ( match stack with
          | x::rest -> eval env (rest, (state, input, output @ [x])) rest_instr )

     | LD x -> eval env ((state x) :: stack, c) rest_instr

     | ST x ->
        ( match stack with
          | z::rest -> eval env (rest, ((Expr.update x z state), input, output)) rest_instr )

     | LABEL l -> eval env config rest_instr

     | JMP l -> eval env config (env#labeled l)

     | CJMP (b, l) ->
        ( match stack with
          | x::rest ->
              if (x = 0 && b = "z" || x != 0 && b = "nz")
              then eval env (rest, c) (env#labeled l)
              else eval env (rest, c) rest_instr )

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o


let label =
  object
    val mutable counter = 0
    method create =
      counter <- counter + 1;
      "l_" ^ string_of_int counter
  end


(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let rec compile stmt =

  let rec compile_expr = function
    | Expr.Var   x            -> [LD x]
    | Expr.Const n            -> [CONST n]
    | Expr.Binop (op, e1, e2) -> 
        (compile_expr e1) 
      @ (compile_expr e2) 
      @ [BINOP op]
  in


  let rec compile_if stmt l_end = match stmt with
    | Stmt.Seq (s1, s2)   -> 
        (compile s1) 
      @ (compile_if s2 l_end)

    | Stmt.If (e, s1, s2) ->
      let l_else = label#create in
        (compile_expr e)
      @ [CJMP ("z", l_else)]
      @ (compile_if s1 l_end)
      @ [JMP l_end]
      @ [LABEL l_else]
      @ (compile s2)

    | _ -> compile stmt
  in


  match stmt with
    | Stmt.Seq (s1, s2) -> 
        (compile s1) 
      @ (compile s2)

    | Stmt.Read x -> 
        [READ]
      @ [ST x]

    | Stmt.Write e -> 
        (compile_expr e) 
      @ [WRITE]

    | Stmt.Assign (x, e) -> 
        (compile_expr e) 
      @ [ST x]

    | Stmt.Skip -> 
        []

    | Stmt.If (e, s1, s2) ->
      let l_end = label#create in
        (compile_if stmt l_end)
      @ [LABEL l_end]

    | Stmt.While (e, s) ->
      let l_expr = label#create in
      let l_od = label#create in
        [JMP l_expr]
      @ [LABEL l_od]
      @ (compile s)
      @ [LABEL l_expr]
      @ (compile_expr e)
      @ [CJMP ("nz", l_od)] 

    | Stmt.Repeat (e, s) ->
      let l_repeat = label#create in
        [LABEL l_repeat] 
      @ (compile s) 
      @ (compile_expr e) 
      @ [CJMP ("z", l_repeat)]

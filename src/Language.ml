(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    let to_bool (i: int): bool = i != 0
    let to_int (b: bool): int = if b then 1 else 0

    let calc (op: string) (left: int) (right: int): int = match op with
      | "+" -> left + right
      | "-" -> left - right
      | "*" -> left * right
      | "/" -> left / right
      | "%" -> left mod right
      | "<" -> to_int (left < right)
      | ">" -> to_int (left > right)
      | "<=" -> to_int (left <= right)
      | ">=" -> to_int (left >= right)
      | "==" -> to_int (left == right)
      | "!=" -> to_int (left != right)
      | "&&" -> to_int ( (to_bool left) && (to_bool right) )
      | "!!" -> to_int ( (to_bool left) || (to_bool right) )

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let rec eval (st: state) (ex: t): int = match ex with
      | Const x -> x
      | Var x -> st x
      | Binop (op, left, right) -> calc op (eval st left) (eval st right)

    let prsBinOp op = ostap(- $(op)), (fun l r -> Binop (op, l, r))
    
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (
      parse: expr;
      expr:
        !(Ostap.Util.expr
          (fun x -> x)
          (Array.map (fun (asc, ops) -> asc, List.map prsBinOp ops)
            [|
              `Lefta, ["!!"];
              `Lefta, ["&&"];
              `Nona , ["<="; "<"; ">="; ">"; "=="; "!="];
              `Lefta, ["+"; "-"];
              `Lefta, ["*"; "/"; "%"];
            |]
          )
          primary
        );
      primary: v:IDENT {Var v} | c:DECIMAL {Const c} | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of Expr.t * t with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (cfg: config) (op: t): config = 
      let (st, input, output) = cfg in
      match op with
      | Read var        -> (match input with
                              | x::rest -> (Expr.update var x st), rest, output 
                              | [] -> failwith("No more input")
                           )

      | Write expr      -> st, input, (output @ [Expr.eval st expr])

      | Assign (var, expr) -> (Expr.update var (Expr.eval st expr) st), input, output

      | Seq (t1, t2)       -> eval (eval cfg t1) t2

      | Skip -> cfg

      | If (expr, stmt1, stmt2) -> eval cfg (if Expr.eval st expr != 0 then stmt1 else stmt2)

      | While (expr, stmt) ->
          if Expr.eval st expr != 0 then eval (eval cfg stmt) op else cfg
      
      | Repeat (expr, stmt) ->
          let ((st', _, _) as cfg') = eval cfg stmt in
          if Expr.eval st' expr = 0 then eval cfg' op else cfg'
                               
    (* Statement parser *)
    ostap (
      parse  : seq | stmt;

      seq    : s1:stmt -";" s2:parse { Seq(s1, s2) };

      stmt   : read | write | assign | skip | if' | for' | while' | repeat';

      read   : %"read" -"(" x:IDENT -")" { Read x };

      write  : %"write" -"(" e:!(Expr.parse) -")" { Write e };

      assign : x:IDENT -":=" e:!(Expr.parse) { Assign (x, e) };

      skip   : %"skip" { Skip };

      if'    : %"if" e:!(Expr.parse)
               %"then" s1:parse
                 elif' :(%"elif" !(Expr.parse) %"then" parse)*
                 else' :(%"else" parse)? %"fi"
                   {
                     let else'' = match else' with
                       | Some t -> t
                       | None -> Skip
                     in
                     let else''' = List.fold_right (fun (e', t') t -> If (e', t', t)) elif' else'' in
                     If (e, s1, else''')
                   };

      for'   : %"for" s1:parse "," e:!(Expr.parse) ","
               s2:parse %"do" s3:parse %"od" { Seq (s1, While (e, Seq (s3, s2))) };

      while' : %"while" e:!(Expr.parse)
               %"do" s:parse %"od" { While (e, s) };

      repeat': %"repeat" s:parse %"until"
                e:!(Expr.parse) { Repeat (e, s) }
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     

exception RuntimeError of string
exception TypeError of string

module EnvKey = 
  struct
    type t = string
    let compare = Stdlib.compare
  end

(* Map of string -> 'a *)
(* Used as envrionment binding *)
module Env = Map.Make (EnvKey)

(* Check whether lhs and rhs matches a particular type *)
(* Raise TypeError when types do not match expectation *)
(* op       : name of the operator 
   t1       : lhs type
   t2       : rhs type
   expected : expected type of t1 and t2
*)
(* Returns  : expected type when both sides match the expected type *)
let type_check (op : string) (t1 : Syntax.ty) (t2 : Syntax.ty) (expected : Syntax.ty): Syntax.ty = 
  if (t1 == expected) && (t2 == expected)
  then expected
  else raise (TypeError (op ^ " expression is ill-typed"))

(* Infer the type of an expression *)
(* Raise TypeError when exp is ill-formed *)
(* 
   senv : static environment (Var -> Type)
   exp  : the expression to infer
*)
(* Returns : the infered type of exp when exp is a well-formed expression *)
let rec type_infer_expr (senv : Syntax.ty Env.t) (exp : Syntax.expr) : Syntax.ty =
  match exp with
    | Syntax.Literal _ -> Syntax.Integer
    | Syntax.Bool _    -> Syntax.Boolean
    | Syntax.Var x     -> Env.find x senv
    | Syntax.Add (e1, e2) -> type_check "Add" (type_infer_expr senv e1) (type_infer_expr senv e2) Syntax.Integer
    | Syntax.And (e1, e2) -> type_check "And" (type_infer_expr senv e1) (type_infer_expr senv e2) Syntax.Boolean

(* Infer the type of a Binding *)
(* Raise TypeError when the expression of the binding is ill-formed *)
(* 
   senv : Static environment (Var -> Type)
   b    : the binding to infer
*)
(* Returns : the pair of the new static environment
and the infered type of the expression of the binding *)
let type_infer_binding (senv : Syntax.ty Env.t) (b : Syntax.binding) 
                            : Syntax.ty * Syntax.ty Env.t =
  match b with
    | Binding (None, e)    -> (type_infer_expr senv e, senv)
    | Binding (Some id, e) -> let ty = type_infer_expr senv e 
                              in (ty, Env.add id ty senv)

(* Evaluate Add operation *)
(* Raises RuntimeError when operands are not integers *)
(* 
   v1 : lhs operand
   v2 : rhs operand
*)
(* Returns : the sum of v1 and v2 in target language *)
let eval_add v1 v2 = 
  match (v1, v2) with
    | (Syntax.VInt x, Syntax.VInt y) -> Syntax.VInt(x + y)
    | _ -> raise (RuntimeError "Adding non-integer values")

(* Evaluate And operation *)
(* Raises RuntimeError when operands are not booleans *)
(* 
   v1 : lhs operand
   v2 : rhs operand
*)
(* Returns : v1 && v2 in target language *)
let eval_and v1 v2 = 
  match (v1, v2) with
    | (Syntax.VBool x, Syntax.VBool y) -> Syntax.VBool (x && y)
    | _ -> raise (RuntimeError "AndOp on non-boolean values")

(* Evaluate an expression *)
(* Raise RuntimeError when the expression is ill-formed *)
(*
   denv : the dynamic environment (Var -> Value)
   e    : the expression to evaluate
*)
(* Returns : the result of evaluating the expression in target language *)
let rec eval_expr (denv : Syntax.value Env.t)(e : Syntax.expr) : Syntax.value =
  match e with
    | Syntax.Literal n -> n
    | Syntax.Bool n -> n
    | Syntax.Var x  -> Env.find x denv
    | Syntax.Add (e1, e2) -> eval_add (eval_expr denv e1) (eval_expr denv e2)
    | Syntax.And (e1, e2) -> eval_and (eval_expr denv e1) (eval_expr denv e2)

(* Evaluate an expression *)
(* Raise RuntimeError when the expression of the binding is ill-formed *)
(*
   denv : the dynamic environment (Var -> Value)
   b    : the binding to evaluate
*)
(* Returns : a pair of a new dynamic environment
and the value of evaluating the expression of the binding in target language *)
let eval_binding (denv : Syntax.value Env.t) (b : Syntax.binding)
                       : Syntax.value * Syntax.value Env.t = 
  match b with
    | Binding (None, e) -> (eval_expr denv e, denv)
    | Binding (Some id, e) -> let res = eval_expr denv e
                              in (res, Env.add id res denv)

let string_of_lex_pos =
  let open Lexing in
  function {pos_fname; pos_lnum; pos_bol; pos_cnum } ->
    Printf.sprintf "%s:%d:%d" pos_fname pos_lnum (pos_cnum - pos_bol)

let get_lexbuf () =
  let lexbuf = Lexing.from_channel stdin in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = "<stdin>" } in
  lexbuf

let () =
  let lexbuf = get_lexbuf () in
  let rec loop (senv : Syntax.ty Env.t) (denv : Syntax.value Env.t) =
    match Parser.main Lexer.token lexbuf with
    | None -> print_endline "bye!"
    | Some e ->
        try
          let (ty, new_senv) = type_infer_binding senv e in
          let (res, new_denv) = eval_binding denv e in
          match e with
            | Binding (None, _) -> let () = Printf.printf "%s\n" (Syntax.show_value res)
                                   in let () = flush stdout in loop new_senv new_denv
            | Binding (Some id, _) -> let () = Printf.printf "%s = %s : %s\n" id (Syntax.show_value res) (Syntax.show_ty ty);
                                   in let () = flush stdout in loop new_senv new_denv
        with TypeError err ->
          print_endline err;
          loop senv denv
  in
  try
    loop Env.empty Env.empty
  with
  | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (string_of_lex_pos pos) msg
  | Parser.Error -> Printf.printf "%s: parse error\n%!" (string_of_lex_pos (Lexing.lexeme_start_p lexbuf))

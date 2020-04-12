exception RuntimeError of string * Lexing.position
exception TypeError of string

module EnvKey = 
  struct
    type t = string
    let compare = Stdlib.compare
  end

(* Map of string -> 'a *)
(* Used as envrionment binding *)
module Env = Map.Make (EnvKey)

type heap = string Env.t * string Env.t

(* let rec type_infer_expr (senv : Syntax.ty Env.t) (expr : Syntax.expr)
                                      : (Syntax.ty * Syntax.ty Env.t) =
  match expr with
    | {loc} *)

(* Evaluate Add operation *)
(* Raises RuntimeError when operands are not integers *)
(* 
   v1 : lhs operand
   v2 : rhs operand
*)
(* Returns : the sum of v1 and v2 in target language *)
let eval_aexp pos op v1 v2 = 
  match (v1, v2) with
    | (Syntax.VInt x, Syntax.VInt y) -> Syntax.VInt(op x y)
    | _ -> raise (RuntimeError ("Integer arith on non-integer values", pos))

(* Evaluate And operation *)
(* Raises RuntimeError when operands are not booleans *)
(* 
   v1 : lhs operand
   v2 : rhs operand
*)
(* Returns : v1 && v2 in target language *)
let eval_bexp pos op v1 v2 = 
  match (v1, v2) with
    | (Syntax.VBool x, Syntax.VBool y) -> Syntax.VBool (op x y)
    | _ -> raise (RuntimeError ("AndOp on non-boolean values", pos))

let eval_relInt pos op x y =
  match op with
    | Syntax.Eq -> Syntax.VBool (x == y)
    | Syntax.Le -> Syntax.VBool (x < y)
    | _         -> raise (RuntimeError ("Unsupported Operator", pos))

let eval_relBool pos op x y =
  match op with
    | Syntax.Eq -> Syntax.VBool (x == y)
    | _         -> raise (RuntimeError ("Unsupported Operator", pos))

let eval_relexp pos op v1 v2 =
  match (v1, v2) with
    | (Syntax.VInt x, Syntax.VInt y) -> eval_relInt pos op x y
    | (Syntax.VBool x, Syntax.VBool y) -> eval_relBool pos op x y
    | (_, _) -> raise (RuntimeError ("Comparing value type mismatched", pos))

let rec eval_binop pos denv e1 op e2 =
  match op with
    | Syntax.Add -> eval_aexp pos (+) (eval_expr denv e2) (eval_expr denv e1) 
    | Syntax.Sub -> eval_aexp pos(-) (eval_expr denv e1) (eval_expr denv e2)
    | Syntax.And -> eval_bexp pos (&&) (eval_expr denv e1) (eval_expr denv e2)
    | Syntax.Eq  -> eval_relexp pos Syntax.Eq (eval_expr denv e1) (eval_expr denv e2)
    | Syntax.Le  -> eval_relexp pos Syntax.Le (eval_expr denv e1) (eval_expr denv e2)

and eval_unop pos denv op e =
  match op with
    | Syntax.Neg -> (match (eval_expr denv e) with
                      | Syntax.VInt x -> Syntax.VInt (0 - x)
                      | _ -> raise (RuntimeError ("Substraction type error", pos)))
    | Syntax.Not -> (match (eval_expr denv e) with
                      | Syntax.VBool x -> Syntax.VBool (not x)
                      | _ -> raise (RuntimeError ("Negation type error", pos)))

and eval_expr (denv : Syntax.value Env.t)(e : Syntax.expr) : Syntax.value =
  match e with
    | {loc = loc; value = stmt} -> 
      let (_, pos) = loc in
          match stmt with
            | Syntax.Literal x -> x
            | Syntax.Var x  -> (match (Env.find_opt x denv) with
                                  | None -> raise (RuntimeError (("Use of undefined variable " ^ x), pos))
                                  | Some v -> v)
            | Syntax.Binop (e1, op, e2) -> eval_binop pos denv e1 op e2
            | Syntax.Unop  (op, e)       -> eval_unop pos denv op e

let rec eval_stmt (denv : Syntax.value Env.t) (stmt : Syntax.stmt) : Syntax.value Env.t =
    match stmt with
      | {loc = _; value = e} -> 
        try
          match e with
            | Syntax.Skip -> denv
            | Syntax.Assign (id, expr) -> Env.add id (eval_expr denv expr) denv
            | Syntax.Seq (expr_l, expr_r) -> eval_stmt (eval_stmt denv expr_l) expr_r
        with RuntimeError (err, pos) -> 
          let () = Printf.printf "RuntimeError: %s, at %s\n" err (Syntax.string_of_lex_pos pos) in
          denv

let get_lexbuf () =
  let lexbuf = Lexing.from_channel stdin in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = "<stdin>" } in
  lexbuf

let rec print_heap heap_bindings =
  match heap_bindings with
    | []                 -> ""
    | (id, result) :: [] -> Printf.sprintf "(\"%s\", %s)" id (Syntax.show_value result)
    | (id, result) :: remaining -> Printf.sprintf "(\"%s\", %s); %s" id (Syntax.show_value result) (print_heap remaining)

let () =
  let lexbuf = get_lexbuf () in
  let stmt = try
      Parser.main Lexer.token lexbuf
    with
    | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (Syntax.string_of_lex_pos pos) msg; exit 1
    | Parser.Error -> Printf.printf "%s: parse error while looking at %s\n%!" (Syntax.string_of_lex_pos (Lexing.lexeme_start_p lexbuf)) (Lexing.lexeme lexbuf); exit 1
  in
  (* print_endline (Syntax.show_stmt stmt); *)
  let denv = Env.empty in
  let result_heap = eval_stmt denv stmt in
  print_endline (print_heap (Env.bindings result_heap))

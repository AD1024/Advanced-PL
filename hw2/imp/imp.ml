exception RuntimeError of string * Lexing.position
exception TypeError of string * Lexing.position
exception AssertionError of string * Lexing.position

module EnvKey = 
  struct
    type t = string
    let compare = Stdlib.compare
  end

(* Map of string -> 'a *)
(* Used as envrionment binding *)
module Env = Map.Make (EnvKey)

type heap = string Env.t * string Env.t

let check_type_1 pos senv t expected msg =
  if t != expected
  then raise (TypeError (msg, pos))
  else senv

let type_check_binop pos op t1 t2 expected ret =
  if (t1 == expected && t2 == expected) then ret
  else raise (TypeError (Printf.sprintf "TypeError: Cannot apply %s on type %s and %s" 
                                      op (Syntax.show_ty t1) (Syntax.show_ty t2), pos))

let type_check_unop pos op t expected ret =
  if t == expected then ret
  else raise (TypeError (Printf.sprintf "Cannot apply %s with %s" op (Syntax.show_ty t), pos))

let rec type_infer_binop pos senv e1 op e2 =
  match op with
    | Syntax.Add -> 
            (match ((type_infer_expr senv e1), (type_infer_expr senv e2)) with
              | (t1, t2) -> type_check_binop pos "Add" t1 t2 Syntax.TInt Syntax.TInt)
    | Syntax.Sub ->
            (match ((type_infer_expr senv e1), (type_infer_expr senv e2)) with
              | (t1, t2) -> type_check_binop pos "Sub" t1 t2 Syntax.TInt Syntax.TInt)
    | Syntax.And ->
            (match ((type_infer_expr senv e1), (type_infer_expr senv e2)) with
              | (t1, t2) -> type_check_binop pos "And" t1 t2 Syntax.TBool Syntax.TBool)
    | Syntax.Eq  ->
        (match ((type_infer_expr senv e1), (type_infer_expr senv e2)) with
                | (t1, t2) -> if t1 == t2 
                              then Syntax.TBool
                              else raise (TypeError 
                                          (Printf.sprintf "Cannot Compare %s with %s" 
                                                (Syntax.show_ty t1) (Syntax.show_ty t2),
                                          pos)))
    | Syntax.Le  ->
        (match ((type_infer_expr senv e1), (type_infer_expr senv e2)) with
              | (t1, t2) -> type_check_binop pos "Le" t1 t2 Syntax.TInt Syntax.TBool)

and type_infer_unop pos senv op e =
  match op with
    | Syntax.Neg -> type_check_unop pos "Neg" (type_infer_expr senv e) Syntax.TInt Syntax.TInt
    | Syntax.Not -> type_check_unop pos "Neg" (type_infer_expr senv e) Syntax.TBool Syntax.TBool

and type_infer_expr (senv : Syntax.ty Env.t) (expr : Syntax.expr) : Syntax.ty =
  match expr with
    | {loc = loc; value = e} ->
        let (_, pos) = loc in
        match e with
          | Syntax.Literal v -> 
                (match v with
                  | Syntax.VBool _ -> Syntax.TBool
                  | Syntax.VInt _  -> Syntax.TInt
                  | Syntax.VUnit   -> Syntax.TUnit)
          | Syntax.Var x     -> 
                (match (Env.find_opt x senv) with
                  | None -> raise (TypeError ("Use of undefined variable " ^ x, pos))
                  | Some ty -> ty)
          | Syntax.Binop (e1, op, e2) -> type_infer_binop pos senv e1 op e2
          | Syntax.Unop (op, e)       -> type_infer_unop pos senv op e
          | Syntax.Ite (cond, lb, rb) ->
                let _ = check_type_1 pos senv (type_infer_expr senv cond) Syntax.TBool 
                        "If condition is not a bool" in
                let t1 = type_infer_expr senv lb in
                (match rb with
                  | None     -> t1
                  | Some rb' -> if (type_infer_expr senv rb') != t1
                                then raise (TypeError ("If-then branch type mismatched", pos))
                                else t1)


let eval_aexp pos op v1 v2 = 
  match (v1, v2) with
    | (Syntax.VInt x, Syntax.VInt y) -> Syntax.VInt(op x y)
    | _ -> raise (RuntimeError ("Integer arith on non-integer values", pos))

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
            | Syntax.Var x  -> 
                    (match (Env.find_opt x denv) with
                      | None -> raise (RuntimeError (("Use of undefined variable " ^ x), pos))
                      | Some v -> v)
            | Syntax.Binop (e1, op, e2)   -> eval_binop pos denv e1 op e2
            | Syntax.Unop  (op, e)        -> eval_unop pos denv op e
            | Syntax.Ite   (cond, lb, rb) ->
                    (match (eval_expr denv cond) with
                      | (Syntax.VBool true) -> eval_expr denv lb
                      | (Syntax.VBool false) ->
                                (match rb with
                                  | None     -> Syntax.VUnit
                                  | Some rb' -> eval_expr denv rb')
                      | _ -> raise (RuntimeError (("If condition is not a bool", pos))))

let rec eval_stmt (denv : Syntax.value Env.t) (stmt : Syntax.stmt) : Syntax.value Env.t =
    match stmt with
      | {loc = loc; value = e} -> 
        let (line, pos) = loc in
        try
          match e with
            | Syntax.Skip -> denv
            | Syntax.Assign (id, expr) -> Env.add id (eval_expr denv expr) denv
            | Syntax.Seq (expr_l, expr_r) -> eval_stmt (eval_stmt denv expr_l) expr_r
            | Syntax.Assert e          -> 
                    (match (eval_expr denv e) with
                      | (Syntax.VBool x) -> 
                            if x
                            then denv
                            else raise (AssertionError ("Assertion failed at ", line))
                      | _ -> raise (RuntimeError ("Assertion can only make on Boolean", pos)))
            | Syntax.While (cond, body) ->
                    (match (eval_expr denv cond) with
                      | (Syntax.VBool true) -> 
                              let denv' = eval_stmt denv body in
                              eval_stmt denv' stmt
                      | (Syntax.VBool false) -> denv
                      | _ -> raise (RuntimeError ("while condition is not a bool", pos)))
        with 
          | RuntimeError (err, pos)   -> 
                    let () = Printf.printf "RuntimeError: %s, at %s\n" 
                                            err (Syntax.string_of_lex_pos pos) in
                    Env.empty
          | AssertionError (err, pos) ->
                    let () = Printf.printf "AssertionError: %s, at %s\n" 
                                            err (Syntax.string_of_lex_pos pos) in
                    Env.empty

let rec type_infer_stmt (senv : Syntax.ty Env.t) (stmt : Syntax.stmt) : Syntax.ty Env.t =
  match stmt with
    | {loc = loc; value = e} ->
        let (_, pos) = loc in
          match e with
            | Syntax.Skip -> senv
            | Syntax.Assign (x, expr) -> 
                      (match (Env.find_opt x senv) with
                        | None -> Env.add x (type_infer_expr senv expr) senv
                        | Some ty -> 
                              let e_type = type_infer_expr senv expr in
                              check_type_1 pos senv ty e_type 
                                      (Printf.sprintf "Cannot unify %s with %s"
                                                      (Syntax.show_ty e_type)
                                                      (Syntax.show_ty ty)))
            | Syntax.Seq (e1, e2) -> type_infer_stmt (type_infer_stmt senv e1) e2
            | Syntax.Assert e     -> let t = type_infer_expr senv e in
                                     if t != Syntax.TBool
                                     then raise (TypeError ("Not asserting on bool", pos))
                                     else senv
            | Syntax.While (cond, body) -> 
                      let msg = Printf.sprintf "While condition is not bool" in
                      let senv' = check_type_1 pos senv (type_infer_expr senv cond) Syntax.TBool msg in
                      type_infer_stmt senv' body
                      

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
  try
    let _ = type_infer_stmt Env.empty stmt in
    let result_heap = eval_stmt denv stmt in
    print_endline (print_heap (Env.bindings result_heap))
  with TypeError (err, pos) ->
          Printf.printf "TypeError: %s, at %s\n" err (Syntax.string_of_lex_pos pos)

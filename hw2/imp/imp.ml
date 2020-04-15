exception RuntimeError of string * Lexing.position
exception TypeError of string * Lexing.position
exception EnvError  of string * Lexing.position
exception AssertionError of string * Lexing.position

module EnvKey = 
  struct
    type t = string
    let compare = Stdlib.compare
  end

(* Map of string -> 'a *)
(* Used as envrionment binding *)
module Env = Map.Make (EnvKey)

type 'a scope = 'a Env.t list

let rec print_heap heap_bindings =
  match heap_bindings with
    | []                 -> ""
    | (id, result) :: [] -> Printf.sprintf "(\"%s\", %s)" id (Syntax.show_value result)
    | (id, result) :: remaining -> Printf.sprintf "(\"%s\", %s); %s" id (Syntax.show_value result) (print_heap remaining)


let check_type_1 pos senv t expected msg =
  if t != expected
  then raise (TypeError (msg, pos))
  else senv

let type_check_binop pos op t1 t2 expected ret =
  if (t1 == expected && t2 == expected) then ret
  else raise (TypeError (Printf.sprintf "Cannot apply %s on type %s and %s" 
                                      op (Syntax.show_ty t1) (Syntax.show_ty t2), pos))

let type_check_unop pos op t expected ret =
  if t == expected then ret
  else raise (TypeError (Printf.sprintf "Cannot apply %s with %s" op (Syntax.show_ty t), pos))

let rec env_lookup (x : string) (env : 'a scope) : 'a option =
  match env with
    | [] -> None
    | cur :: env' -> 
        (match Env.find_opt x cur with
          | None -> env_lookup x env'
          | somex -> somex)

let env_update (x : string) (v : 'a) (env : 'a scope) : 'a scope=
  let rec find dep xs =
    match xs with
      | [] -> -1
      | binding :: xs' ->
        match (Env.find_opt x binding) with
          | None -> find (dep + 1) xs'
          | _    -> dep in
  let rec update dep index xs =
    match xs with
      | [] -> []
      | hd :: tl ->
        if dep == index
        then (Env.add x v hd) :: tl
        else hd :: (update (dep + 1) index tl) in
  let idx = find 0 env in
  if idx == -1
  then 
    match env with
      | [] -> []
      | hd :: tl -> (Env.add x v hd) :: tl
  else update 0 idx env

let rec type_infer_binop pos env e1 op e2 =
  match env with
    | [] -> raise (EnvError ("empty static environment", pos))
    | senv ->
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
        | Syntax.Lt  ->
            (match ((type_infer_expr senv e1), (type_infer_expr senv e2)) with
                  | (t1, t2) -> type_check_binop pos "Lt" t1 t2 Syntax.TInt Syntax.TBool)

and type_infer_unop pos env op e =
  match env with
    | [] -> raise (EnvError ("empty static environment", pos))
    | senv ->
      match op with
        | Syntax.Neg -> type_check_unop pos "Neg" (type_infer_expr senv e) Syntax.TInt Syntax.TInt
        | Syntax.Not -> type_check_unop pos "Not" (type_infer_expr senv e) Syntax.TBool Syntax.TBool

and check_rec_type_eq t1 t2 =
  match (t1, t2) with
    | (Syntax.TArray t1', Syntax.TArray t2') -> check_rec_type_eq t1' t2'
    | _, _ -> t1 == t2

and type_infer_array pos env es =
  (match es with
    | [] -> Syntax.TArray Syntax.TVar
    | e :: es' -> Syntax.TArray (List.fold_left
                (fun e1 e2 -> if check_rec_type_eq e1 e2
                              then e1
                              else raise (TypeError ("type mismatched in array", pos)))
                (type_infer_expr env e)
                (List.map (type_infer_expr env) es')))

and type_infer_expr (senv : Syntax.ty scope) (expr : Syntax.expr) : Syntax.ty =
  match expr with
    | {loc = loc; value = e} ->
        let (_, pos) = loc in
        match e with
          | Syntax.Literal v -> 
                (match v with
                  | Syntax.VBool _ -> Syntax.TBool
                  | Syntax.VInt _  -> Syntax.TInt
                  | Syntax.VUnit   -> Syntax.TUnit
                  | Syntax.VArray _ -> raise (TypeError ("Absurd. Array value should be in runtime", pos)))
          | Syntax.Var x     -> 
              (match senv with
                | [] -> raise (EnvError ("empty static envrionment", pos))
                | senv' ->
                    (match (env_lookup x senv') with
                      | None -> raise (TypeError ("Use of undefined variable " ^ x, pos))
                      | Some ty -> ty))
          | Syntax.Binop (e1, op, e2) -> type_infer_binop pos senv e1 op e2
          | Syntax.Unop (op, e)       -> type_infer_unop pos senv op e
          | Syntax.Array es -> type_infer_array pos senv es
          | Syntax.Subscript (e, i) ->
              (match ((type_infer_expr senv e), (type_infer_expr senv i)) with
                | (Syntax.TArray ty, Syntax.TInt) -> ty
                | (ty, _) -> raise (TypeError (Printf.sprintf "Not a subscritable type %s" (Syntax.show_ty ty), pos)))

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
    | Syntax.Lt -> Syntax.VBool (x < y)
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

let rec eval_binop pos env e1 op e2 =
  match env with
    | [] -> raise (EnvError ("empty static environment", pos))
    | denv ->
      match op with
        | Syntax.Add -> eval_aexp pos (+) (eval_expr denv e2) (eval_expr denv e1) 
        | Syntax.Sub -> eval_aexp pos(-) (eval_expr denv e1) (eval_expr denv e2)
        | Syntax.And -> eval_bexp pos (&&) (eval_expr denv e1) (eval_expr denv e2)
        | Syntax.Eq  -> eval_relexp pos Syntax.Eq (eval_expr denv e1) (eval_expr denv e2)
        | Syntax.Lt  -> eval_relexp pos Syntax.Lt (eval_expr denv e1) (eval_expr denv e2)

and eval_unop pos env op e =
  match env with
    | [] -> raise (EnvError ("empty static environment", pos))
    | denv ->
      match op with
        | Syntax.Neg -> (match (eval_expr denv e) with
                          | Syntax.VInt x -> Syntax.VInt (0 - x)
                          | _ -> raise (RuntimeError ("Substraction type error", pos)))
        | Syntax.Not -> (match (eval_expr denv e) with
                          | Syntax.VBool x -> Syntax.VBool (not x)
                          | _ -> raise (RuntimeError ("Negation type error", pos)))

and eval_expr (denv : Syntax.value scope)(e : Syntax.expr) : Syntax.value =
  match e with
    | {loc = loc; value = stmt} -> 
      let (_, pos) = loc in
          match stmt with
            | Syntax.Literal x -> x
            | Syntax.Var x  -> 
              (match denv with
                | [] -> raise (EnvError ("empty static environment", pos))
                | denv' ->
                    (match (env_lookup x denv') with
                      | None -> raise (RuntimeError (("Use of undefined variable " ^ x), pos))
                      | Some v -> v))
            | Syntax.Binop (e1, op, e2)   -> eval_binop pos denv e1 op e2
            | Syntax.Unop  (op, e)        -> eval_unop pos denv op e
            | Syntax.Array es -> Syntax.VArray (List.map (eval_expr denv) es)
            | Syntax.Subscript (arr, i) -> 
                let varr = eval_expr denv arr in
                let index = eval_expr denv i in
                  (match (varr, index) with
                    | (Syntax.VArray arr, Syntax.VInt idx) ->
                      (match List.nth_opt arr idx with
                        | None -> raise (RuntimeError ("Array out of bound", pos))
                        | Some v -> v)
                    | (_, _) -> raise (RuntimeError ("Not a subscriptable data", pos)))

let rec update_list_elem (dep : int) (x : Syntax.value) (index : int) xs : Syntax.value list =
  if dep == index
  then (match xs with
        | [] -> []
        | _  -> x :: List.tl xs)
  else (match xs with
        | [] -> []
        | x' :: xs' -> x' :: (update_list_elem (dep + 1) x index xs'))

let rec update_list_multi pos (x : Syntax.value) (indices : int list) (xs : Syntax.value) : Syntax.value =
  match indices with
    | [] -> Syntax.VUnit
    | (p :: []) ->
      (match xs with
        | (Syntax.VArray arr) -> Syntax.VArray (update_list_elem 0 x p arr)
        | _ -> raise (RuntimeError ("Not an array", pos)))
    | i :: is' ->
      (match xs with
        | Syntax.VArray arr -> 
            let idx = match List.nth_opt arr i with
                        | None -> raise (RuntimeError (Printf.sprintf "array out of bound %d" i, pos))
                        | Some e -> e in
            Syntax.VArray (update_list_elem 0 (update_list_multi pos x is' idx) i arr)
        | _ -> raise (RuntimeError ("Not an array", pos)))

let rec eval_stmt (env : Syntax.value scope) (stmt : Syntax.stmt) (block : bool) : Syntax.value scope =
  match stmt with
    | {loc = loc; value = e} -> 
      let (line, pos) = loc in
      match env with
        | [] -> raise (EnvError ("empty static environment", pos))
        | _ :: _ ->
            match e with
              | Syntax.Skip -> env
              | Syntax.Assign (id, expr) -> env_update id (eval_expr env expr) env
              | Syntax.Seq (expr_l, expr_r) -> eval_stmt (eval_stmt env expr_l false) expr_r false
              | Syntax.Assert e          -> 
                      (match (eval_expr env e) with
                        | (Syntax.VBool x) -> 
                              if x
                              then env
                              else raise (AssertionError ("Assertion failed", line))
                        | _ -> raise (RuntimeError ("Assertion can only make on Boolean", pos)))
              | Syntax.While (cond, body) ->
                      (match (eval_expr env cond) with
                        | (Syntax.VBool true) -> 
                                let denv' = if not block 
                                            then (eval_stmt (Env.empty :: env) body false)
                                            else eval_stmt env body false in
                                let res = (eval_stmt denv' stmt true) in
                                if not block then List.tl res else res
                        | (Syntax.VBool false) -> env
                        | _ -> raise (RuntimeError ("while condition is not a bool", pos)))
              | Syntax.If (cond, lb, rb) ->
                      (match (eval_expr env cond) with
                        | (Syntax.VBool true) ->
                                let env' = eval_stmt (Env.empty :: env) lb false in
                                List.tl env'
                        | (Syntax.VBool false) -> 
                          (match rb with
                            | None -> env
                            | Some rb' -> 
                                let env' = eval_stmt (Env.empty :: env) rb' false
                                in List.tl env')
                        | _ -> raise (RuntimeError ("if condition is not a bool", pos)))
              | Syntax.AssignArr (id, e) ->
                        let rec get_subscript (s : Syntax.expr) : string * int list =
                          match s with
                            | {loc = (_, pos); value = s'} ->
                            match s' with
                              | Syntax.Subscript (id, i) ->
                                  let idx = eval_expr env i in
                                  (match idx with
                                    | Syntax.VInt idx' ->
                                      (match id with
                                        | {loc = (_, pos'); value = id'} ->
                                          (match id' with
                                            | Syntax.Subscript (subexp, index) -> 
                                                let idx' = eval_expr env index in
                                                (match idx' with
                                                  | Syntax.VInt idx'' -> 
                                                    let (name, subscripts) = get_subscript subexp in
                                                    (name, List.append subscripts [idx''])
                                                  | _ -> raise (RuntimeError ("Not a proper subscript", pos')))
                                            | Syntax.Var x -> (x, [ idx' ])
                                            | _ -> raise (RuntimeError ("Not a proper subscript", pos'))))
                                    | _ -> raise (RuntimeError (Printf.sprintf "%s is not a proper subscript" 
                                                              (Syntax.show_value idx), pos)))
                              | _ -> raise (RuntimeError ("Not a subscript expr", pos)) in
                        let (aname, subscripts) = get_subscript id in
                        let old_arr = env_lookup aname env in
                        (match old_arr with
                          | (Some (Syntax.VArray arr)) -> 
                                  env_update aname (update_list_multi pos (eval_expr env e) subscripts (Syntax.VArray arr)) env
                          | (Some x) -> 
                                  raise (RuntimeError (Printf.sprintf "%s is not subscriptable" (Syntax.show_value x), pos))
                          | (None) -> 
                                  raise (RuntimeError (("Use of undefined variable " ^ aname), pos)))


let rec type_infer_stmt (senv : Syntax.ty scope) (stmt : Syntax.stmt) : Syntax.ty scope =
  match stmt with
    | {loc = loc; value = e} ->
        let (_, pos) = loc in
      match senv with
        | [] -> raise (EnvError ("empty static environment", pos))
        | _ :: _ ->
          match e with
            | Syntax.Skip -> senv
            | Syntax.Assign (x, expr) -> 
                      (match (env_lookup x senv) with
                        | None -> env_update x (type_infer_expr senv expr) senv
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
                      let msg = "While condition is not bool" in
                      let senv' = check_type_1 pos senv (type_infer_expr senv cond) Syntax.TBool msg in
                      let _ = type_infer_stmt senv' body in
                      senv
            | Syntax.If (cond, lb, rb) ->
                      let msg = "If condition is not bool" in
                      let _ = check_type_1 pos senv (type_infer_expr senv cond) Syntax.TBool msg in
                      let _ = type_infer_stmt senv lb in
                      (match rb with
                        | None -> senv
                        | Some rb' -> let _ = type_infer_stmt senv rb' in senv)
            | Syntax.AssignArr (id, e) ->
                      let ty = type_infer_expr senv id in
                      let e_type = type_infer_expr senv e in
                      if check_rec_type_eq ty e_type 
                      then senv else
                            raise (TypeError (Printf.sprintf "Cannot unify %s with %s"
                                            (Syntax.show_ty e_type)
                                            (Syntax.show_ty ty), pos))

let read_back loc (v : Syntax.value) : Syntax.expr =
  {loc = loc; value = Syntax.Literal v}

let rec expr_is_constant env (expr : Syntax.expr) =
  match expr with
    | {loc = _; value = e} ->
      match e with
        | Syntax.Literal _ -> true
        | Syntax.Var x   -> 
          (match Env.find_opt x env with
            | None -> false
            | Some _ -> true)
        | Syntax.Unop (_, e) -> expr_is_constant env e
        | Syntax.Binop (e1, _, e2) -> (expr_is_constant env e1) && (expr_is_constant env e2)
        | Syntax.Array es -> List.fold_left (&&) true (List.map (expr_is_constant env) es)
        | Syntax.Subscript (es, i) -> (expr_is_constant env i) && (expr_is_constant env es)

let rec expr_fold_constant env (expr : Syntax.expr) : Syntax.expr =
  match expr with
    | {loc = loc; value = e} ->
      let (_, pos) = loc in
      match e with
        | Syntax.Literal _ -> expr
        | Syntax.Var x     -> 
          (match Env.find_opt x env with
            | None -> expr
            | Some v -> read_back loc v)
        | Syntax.Binop (e1, op, e2) -> 
          let fe1 = expr_fold_constant env e1 in
          let fe2 = expr_fold_constant env e2 in
          (match (fe1, fe2) with
            | ({loc = _; value = e1'}, {loc = _; value = e2'}) ->
                match (e1', e2') with
                  | (Syntax.Literal _, Syntax.Literal _) -> read_back loc (eval_binop pos [Env.empty] fe1 op fe2)
                  | _              -> {loc = loc; value = Syntax.Binop (fe1, op, fe2)})
        | Syntax.Unop (op, e1) ->
          let e1' = expr_fold_constant env e1 in
          (match e1' with
            | { loc = (_, pos); value = Syntax.Literal _ } -> 
                    { loc = loc; value = Syntax.Literal (eval_unop pos [Env.empty] op e1') }
            | _ -> e1')
        | Syntax.Array es -> {loc = loc; value = (Syntax.Array (List.map (expr_fold_constant env) es))}
        | Syntax.Subscript (arr, i) -> {loc = loc; value = Syntax.Subscript (arr, expr_fold_constant env i)}

let read_back_stmt loc expr : Syntax.stmt = {loc = loc; value = expr}

let rec stmt_fold_constant env (stmt : Syntax.stmt): Syntax.stmt =
  match stmt with
    | {loc = loc; value = s} ->
      match s with
        | Syntax.Skip -> stmt
        | Syntax.Assign (x, e) -> read_back_stmt loc (Syntax.Assign (x, expr_fold_constant env e))
        | Syntax.Seq (e1, e2) ->  read_back_stmt loc (Syntax.Seq (stmt_fold_constant env e1, stmt_fold_constant env e2))
        | Syntax.Assert e     -> {loc = loc; value = Syntax.Assert (expr_fold_constant env e)}
        | Syntax.While (cond, body) ->
            let folded_cond = expr_fold_constant env cond in
            (match folded_cond with
              | {loc = _; value = Syntax.Literal Syntax.VBool false} -> stmt_fold_constant env body
              | _ -> {loc = loc; value = Syntax.While (expr_fold_constant env cond, stmt_fold_constant env body)})
        | Syntax.If (cond, lb, rb) ->
          let folded_cond = expr_fold_constant env cond in
          (match folded_cond with
            | {loc = _; value = Syntax.Literal (Syntax.VBool true)} -> stmt_fold_constant env lb
            | {loc = loc'; value = Syntax.Literal (Syntax.VBool false)} ->
                (match rb with
                  | None -> {loc = loc'; value = Syntax.Skip }
                  | Some rb' -> stmt_fold_constant env rb')
            | _ -> {loc = loc; value = Syntax.If (folded_cond, stmt_fold_constant env lb, 
                (match rb with
                  | None -> None
                  | Some rb' -> Some (stmt_fold_constant env rb')))})
        | Syntax.AssignArr (_, _) -> stmt

let get_lexbuf () =
  let lexbuf = Lexing.from_channel stdin in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = "<stdin>" } in
  lexbuf

let () =
  let lexbuf = get_lexbuf () in
  let stmt = try
      Parser.main Lexer.token lexbuf
    with
    | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (Syntax.string_of_lex_pos pos) msg; exit 1
    | Parser.Error -> Printf.printf "%s: parse error while looking at %s\n%!" (Syntax.string_of_lex_pos (Lexing.lexeme_start_p lexbuf)) (Lexing.lexeme lexbuf); exit 1
  in
  (* print_endline (Syntax.show_stmt stmt); *)
  let denv = [Env.empty] in
  try
    let _ = type_infer_stmt [Env.empty] stmt in
    let folded_ast = stmt_fold_constant Env.empty stmt in
    (* let () = print_endline (Syntax.show_stmt folded_ast) in *)
    try
      let result_heap = eval_stmt denv folded_ast false in
      print_endline (print_heap (Env.bindings (List.hd result_heap)))
    with
      | RuntimeError (err, pos)   -> 
        Printf.printf "RuntimeError: %s, at %s\n" err (Syntax.string_of_lex_pos pos)
      | AssertionError (err, pos) ->
        Printf.printf "AssertionError: %s, at %s\n" err (Syntax.string_of_lex_pos pos)
  with TypeError (err, pos) ->
          Printf.printf "TypeError: %s, at %s\n" err (Syntax.string_of_lex_pos pos)

exception SubstError of string

module StringSet = Set.Make(String)

module EnvKey = 
  struct
    type t = string
    let compare = Stdlib.compare
  end

module Env = Map.Make (EnvKey)

type ctx = Syntax.expr Env.t

let rec unparse (e : Syntax.expr): string= 
  match e.value with
    | Syntax.Var x -> x
    | Syntax.Lam (x, e) -> Printf.sprintf "(\\%s. %s)" x (unparse e)
    | Syntax.App (e1, e2) -> Printf.sprintf "(%s %s)" (unparse e1) (unparse e2)

let free_variables (e : Syntax.expr): StringSet.t=
  let rec free_variable_helper (e : Syntax.expr)(bound : StringSet.t): StringSet.t=
    match e with
      | {loc = _; value = e} ->
        match e with
          | Var x -> 
              (match StringSet.find_opt x bound with
                | None -> StringSet.add x (StringSet.empty)
                | Some _ -> StringSet.empty)
          | Lam (x, expr) -> 
              free_variable_helper expr (StringSet.add x bound)
          | App (e1, e2) ->
              StringSet.union (free_variable_helper e1 bound) (free_variable_helper e2 bound) in
  free_variable_helper e StringSet.empty

let rec subst (x : string) (v : Syntax.expr) (e : Syntax.expr): Syntax.expr=
  match e with
    | {loc = loc; value = e'} ->
      match e' with
        | Var x' -> if String.equal x x' then v else e
        | Lam (x', expr) -> 
          if String.equal x x'
          then e
          else
            let free_vars = free_variables v in
            (match StringSet.find_opt x' free_vars with
              | None -> {loc = loc; value = Lam (x', subst x v expr)}
              | Some _ -> raise (SubstError "Free variable in subst term"))
        | App (e1, e2) -> 
          let e1' = subst x v e1 in
          let e2' = subst x v e2 in
          {loc = loc; value = App (e1', e2')} 

let rec step (e : Syntax.expr): Syntax.expr option=
  match e.value with
    | Var _ -> None
    | Lam _ -> None
    | App (e1, e2) ->
      match step e1 with
        | None ->
            (match step e2 with
              | Some e2' -> 
                (match e1.value with
                  | Lam (x, expr) ->
                      (match StringSet.find_opt x (free_variables expr) with
                        | None -> Some expr
                        | Some _ -> Some ({loc = e1.loc; value = Syntax.App (e1, e2')}))
                  | _ -> Some ({loc = e1.loc; value = Syntax.App (e1, e2')}))
              | None ->
                match e1.value with
                  | Lam (x, expr) -> Some (subst x e2 expr)
                  | _ -> None)
        | Some e1' -> Some ({loc = e1'.loc; value = Syntax.App (e1', e2)})

let rec step_loop (e : Syntax.expr): Syntax.expr=
  match step e with
    | None -> e
    | Some e' -> step_loop e'

let rec subst_env (e : Syntax.expr) (ctx_bindings: (string * Syntax.expr) list) : Syntax.expr=
  match ctx_bindings with
    | [] -> e
    | ((id, expr) :: remaining) -> subst_env (subst id expr e) remaining

let rec step_n (dep : int) (e : Syntax.expr): Syntax.expr =
  let () = print_endline (unparse e);print_endline "" in
  if dep == 0 then e
  else match step e with
        | None -> e
        | Some e' -> step_n (dep - 1) e'

let eval_binding (e : Syntax.binding) (env : ctx): ctx=
  match e with
    | Syntax.Binding (None, e) ->
        let () = print_endline (">>> " ^ (unparse (step_loop (subst_env e (Env.bindings env))))) in
        env
    | Syntax.Binding (Some x, e) ->
        let e' = step_loop (subst_env e (Env.bindings env)) in
        let () = print_endline (Printf.sprintf ">>> %s" (unparse e')) in
        Env.add x e' env

let rec list_to_string (xs : string list): string=
  match xs with
    | [] -> ""
    | (x :: xs) -> x ^ "; " ^ (list_to_string xs)


let get_lexbuf () =
  let lexbuf = Lexing.from_channel stdin in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = "<stdin>" } in
  lexbuf

let () =
  let lexbuf = get_lexbuf () in
  let rec loop (env : ctx) =
    match Parser.main Lexer.token lexbuf with
    | None -> print_endline "bye!"
    | Some e ->
       loop (eval_binding e env)
  in
  try
    loop (Env.empty)
  with
  | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (Syntax.string_of_lex_pos pos) msg; exit 1
  | Parser.Error -> Printf.printf "%s: parse error while looking at %s\n%!" (Syntax.string_of_lex_pos (Lexing.lexeme_start_p lexbuf)) (Lexing.lexeme lexbuf); exit 1




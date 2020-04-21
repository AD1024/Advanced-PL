module StringSet = Set.Make(String)

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
          else {loc = loc; value = Lam (x', subst x v expr)}
        | App (e1, e2) -> 
          let e1' = subst x v e1 in
          let e2' = subst x v e2 in
          {loc = loc; value = App (e1', e2')} 

let rec step (e : Syntax.expr): Syntax.expr option=
  match e.value with
    | Var _ -> None
    | Lam _ -> None
    | App (e1, e2) ->
      match e1.value with
        | Var _ ->
          (match step e2 with
            | None -> None
            | Some e2 -> Some ({loc = e1.loc; value = Syntax.App (e1, e2)}))
        | Lam (x, expr) ->
          (match e2.value with
            | Var _ -> Some (subst x e2 expr)
            | _   ->
              match step e2 with
                | None -> Some (subst x e2 expr)
                | Some e2' -> Some ({loc = e1.loc; value = Syntax.App (e1, e2')}))
        | _ ->
          (match step e1 with
            | None ->
              (match step e2 with
                | None -> None
                | Some e2' -> Some ({loc = e1.loc; value = Syntax.App (e1, e2')}))
            | Some e1' -> Some ({loc = e1'.loc; value = Syntax.App (e1', e2)}))


let rec step_loop (e : Syntax.expr): Syntax.expr=
  match step e with
    | None -> e
    | Some e' -> step_loop e'

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
  let rec loop () =
    match Parser.main Lexer.token lexbuf with
    | None -> print_endline "bye!"
    | Some e ->
       print_endline (Syntax.show_expr e);
       print_endline (list_to_string (StringSet.elements (free_variables e)));
       print_endline (Syntax.show_expr (step_loop e));
       loop ()
  in
  try
    loop ()
  with
  | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (Syntax.string_of_lex_pos pos) msg; exit 1
  | Parser.Error -> Printf.printf "%s: parse error while looking at %s\n%!" (Syntax.string_of_lex_pos (Lexing.lexeme_start_p lexbuf)) (Lexing.lexeme lexbuf); exit 1




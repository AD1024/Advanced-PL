open Hw5lib

exception TypeError of Syntax.location option * string

let rec type_eq (tau_1 : Syntax.Ty.t) (tau_2 : Syntax.Ty.t): bool =
  match (tau_1.value, tau_2.value) with
    | (Syntax.Ty.Bool, Syntax.Ty.Bool) -> true
    | (Syntax.Ty.Fun (t1, t2), Syntax.Ty.Fun(t1', t2')) -> (type_eq t1 t1') && (type_eq t2 t2')
    | (Syntax.Ty.TypeVar v1, Syntax.Ty.TypeVar v2) -> v1 = v2
    | (_, _) -> false

let rec type_infer (delta : string list) (gamma : (string * Syntax.Ty.t) list) (e : Syntax.expr) : Syntax.Ty.t =
  (* delete these lines and implement as a recursive function returning the type of e *)
  (* if e is not well typed, throw a TypeError with appropriate location info and msg. *)
  match e.value with
    | Var x   -> (match List.find_opt (fun (name, _) -> String.equal name x) gamma with
                | None -> raise (TypeError (e.loc, Printf.sprintf "%s is not found in gamma" x))
                | Some ty -> snd ty)
    | Bool _  -> Syntax.with_no_loc Syntax.Ty.Bool
    | Lambda (bound_var, ty, expr') ->
        let e_type = type_infer delta (List.cons (bound_var, ty) gamma) expr' in
        Syntax.with_no_loc (Syntax.Ty.Fun (ty, e_type))
    | App (e1, e2) ->
        let tau_arr = type_infer delta gamma e1 in
        (match tau_arr.value with
          | Syntax.Ty.Fun (tau_1, tau_2) ->
              let () = type_check delta gamma e2 tau_1 in tau_2
          | _ -> raise (TypeError (e.loc, "Cannot apply on non arrow type")))
    | IfThenElse (cond, lb, rb) ->
        let () = type_check delta gamma cond (Syntax.with_no_loc Syntax.Ty.Bool) in
        let ty_lb = type_infer delta gamma lb in
        let () = type_check delta gamma rb ty_lb in ty_lb
    | TypeApp (e, t1) -> 
        let e_ty = type_infer delta gamma e in
        (match e_ty.value with
          | Syntax.Ty.Forall (var, ty) -> Syntax.Ty.subst var t1 ty
          | _ -> raise (TypeError (e.loc, "Cannot apply type on non Forall type")))
    | LAMBDA (ty, e) -> Syntax.with_no_loc (Syntax.Ty.Forall (ty, (type_infer (List.cons ty delta) gamma e)))

(* You might find this helper function useful when defining type_infer. It checks that 
   [type_infer] returns the given expected type. *)
and type_check delta gamma e ty =
  let ty' = type_infer delta gamma e in
  if not (type_eq ty ty')
  then raise (TypeError (e.Syntax.loc, Printf.sprintf "expected type %s but got %s" (Syntax.Ty.pretty ty) (Syntax.Ty.pretty ty')))

let get_lexbuf () =
  let lexbuf = Lexing.from_channel stdin in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = "<stdin>" } in
  lexbuf

let rec step_loop e =
  (* print_endline (Syntax.pretty e); *)
  match Syntax.step e with
  | None -> e
  | Some e' -> step_loop e'

(* kinda like multisubstitution, but implemented via repeated (single) substitution.
   only correct under the assumption that the RHS of every abbreviation contains
   *no* further abbreviations. *)
let subst_all abbrevs e =
  List.fold_left (fun e (from, to_) -> Syntax.subst from to_ e) e abbrevs

let () =
  let lexbuf = get_lexbuf () in
  let rec loop abbrevs =
    let process ox e =
      let e_expanded = subst_all abbrevs e in
       begin try
         ignore (type_infer [] [] e_expanded); (* ignore means just make sure it has *some* type, we don't care what it is. *)
         let e' = step_loop e_expanded in
         print_endline (Syntax.pretty (Syntax.normalize e'));
         match ox with
         | Some x -> loop ((x, e') :: abbrevs)
         | None -> loop abbrevs
       with
       | TypeError (loc, msg) ->
          (* report the error and continue *)
          Printf.printf "%s: type error: %s\n%!" (Syntax.string_of_loc loc) msg;
          loop abbrevs
       end
    in
    match Parser.main Lexer.token lexbuf with
    | None -> print_endline "bye!"
    | Some (Eval e) ->
       print_endline (Syntax.pretty e);
       process None e
    | Some (Val (x, e)) ->
       Printf.printf "%s = %s\n%!" x (Syntax.pretty e);
       process (Some x) e
  in
  try
    loop []
  with
  (* lexical and syntax errors are unrecoverable, so catch them outside the loop. *)
  | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (Syntax.string_of_lex_pos pos) msg; exit 1
  | Parser.Error -> Printf.printf "%s: parse error while looking at %s\n%!" (Syntax.string_of_lex_pos (Lexing.lexeme_start_p lexbuf)) (Lexing.lexeme lexbuf); exit 1
    

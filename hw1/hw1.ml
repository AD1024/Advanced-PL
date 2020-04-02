exception TypeError of string

let eval_add v1 v2 = 
  match (v1, v2) with
    | (Syntax.VInt x, Syntax.VInt y) -> Syntax.VInt(x + y)
    | _ -> raise (TypeError "Adding non-integer values")

let eval_and v1 v2 = 
  match (v1, v2) with
    | (Syntax.VBool x, Syntax.VBool y) -> Syntax.VBool (x && y)
    | _ -> raise (TypeError "AndOp on non-boolean values")

let rec eval (e : Syntax.expr) : Syntax.value =
  match e with
  | Syntax.Literal n -> n
  | Syntax.Bool n -> n
  | Syntax.Add (e1, e2) -> eval_add (eval e1) (eval e2)
  | Syntax.And (e1, e2) -> eval_and (eval e1) (eval e2)


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
  let rec loop () =
    match Parser.main Lexer.token lexbuf with
    | None -> print_endline "bye!"
    | Some e ->
       Printf.printf "%s\n%!" (Syntax.show_value (eval e));
       loop ()
  in
  try
    loop ()
  with
  | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (string_of_lex_pos pos) msg
  | Parser.Error -> Printf.printf "%s: parse error\n%!" (string_of_lex_pos (Lexing.lexeme_start_p lexbuf))

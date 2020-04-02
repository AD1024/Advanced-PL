(* let rec eval (e : Syntax.expr) : 'a value =
  match e with
  | Syntax.Literal n -> n
  | Syntax.Add (e1, e2) -> eval e1 + eval e2
  | Syntax.And (e1, e2) ->  *)


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
       (* Printf.printf "%d\n%!" (eval e); *)
       Printf.printf "%s\n%!" (Syntax.show_expr e);
       loop ()
  in
  try
    loop ()
  with
  | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (string_of_lex_pos pos) msg
  | Parser.Error -> Printf.printf "%s: parse error\n%!" (string_of_lex_pos (Lexing.lexeme_start_p lexbuf))

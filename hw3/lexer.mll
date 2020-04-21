{
  open Parser

  exception Error of Lexing.position * string
}

rule token = parse
| [' ' '\t']                                { token lexbuf }
| '\n'                                      { Lexing.new_line lexbuf; token lexbuf }



(* delete the following line *)
| '\\'                                      { LAM }
| '.'                                       { DOT }
| ['a'-'z''A'-'Z']+ as id                   { ID id }
| '('                                       { LPAREN }
| ')'                                       { RPAREN }
| '='                                       { EQ }
| ";;"                                      { DOUBLESEMI }
| '#' [^'\n']* '\n'                         { token lexbuf }  (* ignore comments (start with #) *)
| eof                                       { EOF }
| _                                         {
    let msg = Printf.sprintf "unexpected character %C" (Lexing.lexeme_char lexbuf 0)
    in raise (Error (Lexing.lexeme_start_p lexbuf, msg))
}

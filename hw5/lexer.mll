{
  open Parser

  exception Error of Lexing.position * string
}

rule token = parse
| [' ' '\t']                                           { token lexbuf }
| '\n'                                                 { Lexing.new_line lexbuf; token lexbuf }
| '\\'                                                 { BACKSLASH }
| '.'                                                  { DOT }
| ':'                                                  { COLON }
| "->"                                                 { ARROW }
| "bool"                                               { BOOL }
| "true"                                               { TRUE }
| "false"                                              { FALSE }
| "if"                                                 { IF }
| "then"                                               { THEN }
| "else"                                               { ELSE }
| '('                                                  { LPAREN }
| ')'                                                  { RPAREN }
| ";;"                                                 { DOUBLESEMI }
| '='                                                  { EQ }
| '#' [^'\n']* '\n'                                    { Lexing.new_line lexbuf; token lexbuf }
| '#' [^'\n']* eof                                     { EOF }
| eof                                                  { EOF }

(* expression variables are restricted to start with lowercase letter or underscore *)
| ['a'-'z''_']['a'-'z''A'-'Z''0'-'9''\'''_']* as x     { ID x }

| _                                         {
    let msg = Printf.sprintf "unexpected character %C" (Lexing.lexeme_char lexbuf 0)
    in raise (Error (Lexing.lexeme_start_p lexbuf, msg))
}

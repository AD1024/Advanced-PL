{
  open Parser

  exception Error of Lexing.position * string
}

rule token = parse
| [' ' '\t']             { token lexbuf }
| '\n'                   { Lexing.new_line lexbuf; token lexbuf }
| ['0'-'9']+ as i        { INT (try int_of_string i with _ -> max_int) }
| "true"                 { BOOL true }
| "false"                { BOOL false }
| "&&"                   { AND }
| '+'                    { PLUS }
| '('                    { LPAREN }
| ')'                    { RPAREN }
| ";;"                   { DOUBLESEMI }
| eof                    { EOF }
| _                      {
    let msg = Printf.sprintf "unexpected character %C" (Lexing.lexeme_char lexbuf 0)
    in raise (Error (Lexing.lexeme_start_p lexbuf, msg))
}

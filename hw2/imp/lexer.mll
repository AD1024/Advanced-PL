{
  open Parser

  exception Error of Lexing.position * string
}

rule token = parse
| [' ' '\t']             { token lexbuf }
| '\n'                   { Lexing.new_line lexbuf; token lexbuf }
| ['0'-'9']+ as i        { match int_of_string_opt i with
                           | None -> raise (Error (Lexing.lexeme_start_p lexbuf, Printf.sprintf "invalid integer '%s' (possibly out of range?)" i))
                           | Some n -> INT n
                         }
| "true"                 { TRUE }
| "false"                { FALSE }
| "skip"                 { SKIP }
| "assert"               { ASSERT }
| "while"                { WHILE }
| "if"                   { IF }
| "then"                 { THEN }
| "else"                 { ELSE }
| "end"                  { END }
| '!'                    { NOT  }
| '+'                    { PLUS }
| '-'                    { MINUS }
| "&&"                   { DOUBLEAMP }
| '{'                    { LCURLY }
| '}'                    { RCURLY }
| '('                    { LPAREN }
| ')'                    { RPAREN }
| '='                    { EQ }
| '<'                    { LE }
| ":="                   { ASSIGN }
| ';'                    { SEMISEP }
| eof                    { EOF }
| ['a'-'z''A'-'Z']+ as x { ID x }
| _                      {
    let msg = Printf.sprintf "unexpected character %C" (Lexing.lexeme_char lexbuf 0)
    in raise (Error (Lexing.lexeme_start_p lexbuf, msg))
}

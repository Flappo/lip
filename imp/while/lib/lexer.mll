{
  open Parser

  exception Error
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| ":=" { ASSIGN }
| "=" { EQ }
| "<=" { LEQ }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "while" { WHILE }
| "do" { DO }
| "skip" { SKIP }
| ";" { SEMICOLON }
| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "(" { LPAREN }
| ")" { RPAREN }
| "true" { TRUE }
| "false" { FALSE }
| "not" { NOT }
| "and" { AND }
| "or" { OR }
| ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']* { ID (Lexing.lexeme lexbuf) }
| ['0'-'9']+ { INT (Lexing.lexeme lexbuf |> int_of_string) }
| _ { raise Error }
| eof { EOF }
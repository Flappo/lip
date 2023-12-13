{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let chars = ['a'-'z''A'-'Z']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { SUB } (* sottrazione *)
  | "*" { MUL } (* moltiplicazione *)
  | "/" { DIV } (* divisione *)
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
(*while exercise*)
  | "skip" { SKIP }
  | "true" { TRUE }
  | "false" { FALSE }
  | chars { VAR (Lexing.lexeme lexbuf) }
  | "and" { AND }
  | "not" { NOT }
  | "=" { EQ }
  | "<=" { LEQ }
  | ":=" { ASSIGN }
  | "if" { IF } (*if then else*)
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | ";" { SEQ }
 
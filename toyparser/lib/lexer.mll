{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*

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
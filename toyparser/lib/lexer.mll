{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hexdigit = ['0'-'9''a'-'f''A'-'F']*
let hex = '0' ('x'|'X') hexdigit* (*numero esadecimale*)

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { SUB } (* sottrazione *)
  | "*" { MUL } (* moltiplicazione *)
  | "/" { DIV } (* divisione *)
  | hex | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
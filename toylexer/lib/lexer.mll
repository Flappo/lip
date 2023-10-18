{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

(*Associazione caratteri e tokens*)
rule read_token =
  parse
  | white { read_token lexbuf }  (*ignora gli spazi tra i token*)
  | "(" { LPAREN } 
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  
  | id { ID (Lexing.lexeme lexbuf) } (*ID avrà dentro una stringa con ciò che viene letto*)
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }

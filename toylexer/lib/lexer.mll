{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

let lvowel = ['a''e''i''o''u']
let uvowel = ['A''E''I''O''U']
let vowel = lvowel|uvowel (*tutte le vocali*)
let cons = letter # vowel (*consonanti*)

let frac = '.' digit* (* .0001, .123, . *)

let hexdigit = ['0'-'9''a'-'f''A'-'F'] (* esadecimali *)

let atok = ['A'-'Z'] chr* (*non accetta stringhe vuote perchè prende solo quelle che iniziano per lettera maiuscola*)
let btok = lvowel+ (*una o più vocali minuscole*)
let ctok = cons* vowel? cons* (*al più una vocale maiucsola o minuscola*)
let dtok = '-'? num frac?
let etok = '0' ['x''X'] (hexdigit hexdigit)+


(*Associazione caratteri e tokens*)
rule read_token =
  parse
  | white { read_token lexbuf }  (*ignora gli spazi tra i token*)
  | "(" { LPAREN } 
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }  

  |atok { ATOK(Lexing.lexeme lexbuf)}
  |btok { ATOK(Lexing.lexeme lexbuf)}
  |ctok { ATOK(Lexing.lexeme lexbuf)}
  |dtok { ATOK(Lexing.lexeme lexbuf)}
  |etok { ATOK(Lexing.lexeme lexbuf)}

  | id { ID (Lexing.lexeme lexbuf) } (*ID avrà dentro una stringa con ciò che viene letto*)
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }

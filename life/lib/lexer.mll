{
 open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']+

 (* Crea il token { TOKEN } quando legge "TOKEN" *)
rule read_token = 
    parse 
    | white { read_token lexbuf }
    | ['E''e'] { E }
    | ['S''s'] { S }
    | ['B''b'] { B }
    | "," { COMMA }
    | ".." { RANGE }
    | "/" { SLASH }
    | num { CONST (Lexing.lexeme lexbuf) }
    | eof { EOF } 
type token =
  | LPAREN
  | RPAREN
  | ASSIGN
  | PLUS
  | SEQ
  | ID of string
  | CONST of string

  | ATOK of string
  | BTOK of string
  | CTOK of string
  | DTOK of string
  | ETOK of string

  | EOF

let string_of_token = function
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | SEQ -> "SEQ"
  | ID(s) -> "ID(" ^ s ^ ")"
  | CONST(s) -> "CONST(" ^ s ^ ")"

  | ATOK(s) -> "ATOK(" ^ s ^ ")"
  | BTOK(s) -> "ATOK(" ^ s ^ ")"
  | CTOK(s) -> "ATOK(" ^ s ^ ")"
  | DTOK(s) -> "ATOK(" ^ s ^ ")"
  | ETOK(s) -> "ATOK(" ^ s ^ ")"

  | EOF -> "EOF"

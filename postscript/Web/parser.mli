type token =
  | IDENTIFIER of (string)
  | LITCONSTANT of (string)
  | STRINGCONSTANT of (string)
  | FLOATCONSTANT of (float)
  | BOOLCONSTANT of (bool)
  | TP of (Lang.tp)
  | INTCONSTANT of (int)
  | BANG
  | NEG
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | MOD
  | PLUSF
  | MINUSF
  | TIMESF
  | DIVF
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EQ
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF_KW
  | ELSE_KW
  | DO_KW
  | WHILE_KW
  | BREAK_KW
  | CONTINUE_KW
  | RETURN_KW
  | BCEQ
  | BCGE
  | BCGT
  | BCLE
  | BCLT
  | BCNE
  | BLAND
  | BLOR
  | EOF
  | FUNDECLDEF

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lang.prog

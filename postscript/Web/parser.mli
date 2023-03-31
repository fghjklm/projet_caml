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
  | VOID_KW
  | INT_KW
  | FLOAT_KW
  | STRING_KW
  | EQ
  | COMMA
  | SEMICOLON
  | COLON
  | QMARK
  | IF_KW
  | ELSE_KW
  | SWITCH_KW
  | FOR_KW
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

val start :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lang.prog

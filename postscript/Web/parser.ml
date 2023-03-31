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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Lang
# 57 "parser.ml"
let yytransl_const = [|
  264 (* BANG *);
  265 (* NEG *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* TIMES *);
  269 (* DIV *);
  270 (* MOD *);
  271 (* PLUSF *);
  272 (* MINUSF *);
  273 (* TIMESF *);
  274 (* DIVF *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* LBRACE *);
  278 (* RBRACE *);
  279 (* VOID_KW *);
  280 (* INT_KW *);
  281 (* FLOAT_KW *);
  282 (* STRING_KW *);
  283 (* EQ *);
  284 (* COMMA *);
  285 (* SEMICOLON *);
  286 (* COLON *);
  287 (* QMARK *);
  288 (* IF_KW *);
  289 (* ELSE_KW *);
  290 (* SWITCH_KW *);
  291 (* FOR_KW *);
  292 (* DO_KW *);
  293 (* WHILE_KW *);
  294 (* BREAK_KW *);
  295 (* CONTINUE_KW *);
  296 (* RETURN_KW *);
  297 (* BCEQ *);
  298 (* BCGE *);
  299 (* BCGT *);
  300 (* BCLE *);
  301 (* BCLT *);
  302 (* BCNE *);
  303 (* BLAND *);
  304 (* BLOR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENTIFIER *);
  258 (* LITCONSTANT *);
  259 (* STRINGCONSTANT *);
  260 (* FLOATCONSTANT *);
  261 (* BOOLCONSTANT *);
  262 (* TP *);
  263 (* INTCONSTANT *);
    0|]

let yylhs = "\255\255\
\001\000\005\000\006\000\007\000\007\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\004\000\004\000\004\000\004\000\009\000\
\009\000\009\000\008\000\008\000\008\000\008\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\000\000"

let yylen = "\002\000\
\001\000\004\000\005\000\000\000\004\000\001\000\002\000\000\000\
\003\000\006\000\003\000\002\000\002\000\005\000\007\000\004\000\
\005\000\001\000\003\000\003\000\001\000\005\000\004\000\000\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\048\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\028\000\029\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\013\000\000\000\018\000\000\000\000\000\002\000\007\000\003\000\
\000\000\000\000\000\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\031\000\033\000\035\000\037\000\039\000\032\000\
\034\000\036\000\038\000\011\000\000\000\040\000\044\000\043\000\
\042\000\041\000\045\000\046\000\047\000\000\000\000\000\000\000\
\000\000\016\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\005\000\026\000\017\000\000\000\000\000\014\000\023\000\
\000\000\010\000\000\000\000\000\015\000"

let yydgoto = "\002\000\
\004\000\018\000\019\000\042\000\005\000\006\000\024\000\025\000\
\043\000\070\000"

let yysindex = "\004\000\
\001\255\000\000\009\255\000\000\000\000\249\254\253\254\002\255\
\006\255\246\254\002\255\255\254\002\255\000\255\248\254\252\254\
\005\255\004\255\002\255\000\000\000\000\000\000\000\000\008\255\
\021\255\005\255\005\255\011\255\005\255\254\254\005\255\000\000\
\000\000\017\255\000\000\005\255\087\255\000\000\000\000\000\000\
\015\255\109\255\024\255\131\255\000\000\170\255\018\255\209\255\
\005\255\248\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\005\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\005\255\006\255\005\255\
\019\255\000\000\002\255\005\255\002\255\029\255\000\000\014\000\
\075\000\000\000\000\000\000\000\002\255\053\000\000\000\000\000\
\005\255\000\000\022\255\075\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\255\
\038\255\000\000\028\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\037\255\000\000\000\000\000\000\000\000\000\000\
\000\000\045\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\065\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\255\000\000\000\000\000\000\000\000\000\000\000\000\
\045\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\255\045\255\
\000\000\000\000\028\255\000\000\028\255\000\000\000\000\000\000\
\027\255\000\000\000\000\000\000\028\255\000\000\000\000\000\000\
\000\000\000\000\000\000\033\255\000\000"

let yygindex = "\000\000\
\000\000\245\255\000\000\240\255\000\000\000\000\253\255\000\000\
\211\255\000\000"

let yytablesize = 379
let yytable = "\028\000\
\037\000\030\000\010\000\078\000\001\000\034\000\003\000\039\000\
\026\000\007\000\044\000\035\000\046\000\008\000\048\000\009\000\
\027\000\029\000\031\000\050\000\032\000\041\000\011\000\036\000\
\033\000\038\000\083\000\040\000\020\000\021\000\022\000\023\000\
\045\000\012\000\047\000\049\000\076\000\013\000\014\000\015\000\
\016\000\017\000\071\000\073\000\080\000\020\000\020\000\084\000\
\088\000\008\000\093\000\022\000\022\000\081\000\020\000\020\000\
\020\000\004\000\006\000\086\000\022\000\022\000\022\000\085\000\
\024\000\087\000\025\000\082\000\000\000\000\000\000\000\000\000\
\092\000\090\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\000\000\021\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\000\021\000\021\000\021\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\021\000\000\000\000\000\060\000\000\000\061\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\000\000\
\072\000\000\000\000\000\061\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\000\000\000\000\074\000\
\000\000\061\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\058\000\059\000\075\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\000\000\077\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\061\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\062\000\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\058\000\059\000\000\000\079\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\061\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\000\000\000\000\000\000\089\000\061\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\058\000\059\000\000\000\
\091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\061\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\058\000\059\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\000\000\000\000\000\000\
\000\000\061\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\062\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000"

let yycheck = "\011\000\
\017\000\013\000\001\001\049\000\001\000\001\001\006\001\019\000\
\019\001\001\001\027\000\007\001\029\000\021\001\031\000\019\001\
\027\001\019\001\019\001\036\000\029\001\001\001\021\001\019\001\
\029\001\022\001\072\000\020\001\023\001\024\001\025\001\026\001\
\022\001\032\001\037\001\019\001\019\001\036\001\037\001\038\001\
\039\001\040\001\028\001\020\001\061\000\019\001\020\001\029\001\
\020\001\022\001\029\001\019\001\020\001\070\000\028\001\029\001\
\030\001\020\001\022\001\076\000\028\001\029\001\030\001\075\000\
\020\001\077\000\020\001\071\000\255\255\255\255\255\255\255\255\
\089\000\085\000\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\020\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\028\001\029\001\030\001\031\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\041\001\042\001\043\001\044\001\045\001\046\001\047\001\
\048\001\255\255\255\255\029\001\255\255\031\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\041\001\
\042\001\043\001\044\001\045\001\046\001\047\001\048\001\255\255\
\028\001\255\255\255\255\031\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\041\001\042\001\043\001\
\044\001\045\001\046\001\047\001\048\001\255\255\255\255\029\001\
\255\255\031\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\041\001\042\001\043\001\044\001\045\001\
\046\001\047\001\048\001\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\031\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\041\001\042\001\043\001\044\001\045\001\046\001\
\047\001\048\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\020\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\031\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\041\001\042\001\043\001\044\001\045\001\046\001\047\001\
\048\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\020\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\031\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\041\001\042\001\043\001\044\001\045\001\046\001\047\001\048\001\
\255\255\255\255\255\255\030\001\031\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\041\001\042\001\
\043\001\044\001\045\001\046\001\047\001\048\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\020\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\031\001\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\041\001\042\001\043\001\
\044\001\045\001\046\001\047\001\048\001\255\255\255\255\255\255\
\255\255\031\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\041\001\042\001\043\001\044\001\045\001\
\046\001\047\001\048\001"

let yynames_const = "\
  BANG\000\
  NEG\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  MOD\000\
  PLUSF\000\
  MINUSF\000\
  TIMESF\000\
  DIVF\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  VOID_KW\000\
  INT_KW\000\
  FLOAT_KW\000\
  STRING_KW\000\
  EQ\000\
  COMMA\000\
  SEMICOLON\000\
  COLON\000\
  QMARK\000\
  IF_KW\000\
  ELSE_KW\000\
  SWITCH_KW\000\
  FOR_KW\000\
  DO_KW\000\
  WHILE_KW\000\
  BREAK_KW\000\
  CONTINUE_KW\000\
  RETURN_KW\000\
  BCEQ\000\
  BCGE\000\
  BCGT\000\
  BCLE\000\
  BCLT\000\
  BCNE\000\
  BLAND\000\
  BLOR\000\
  EOF\000\
  "

let yynames_block = "\
  IDENTIFIER\000\
  LITCONSTANT\000\
  STRINGCONSTANT\000\
  FLOATCONSTANT\000\
  BOOLCONSTANT\000\
  TP\000\
  INTCONSTANT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fundefn) in
    Obj.repr(
# 38 "parser.mly"
               (  Prog ([], [_1]) )
# 342 "parser.ml"
               : Lang.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fundecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Lang.com) in
    Obj.repr(
# 43 "parser.mly"
  ( Fundefn(_1, _3) )
# 350 "parser.ml"
               : 'fundefn))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Lang.tp) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'vardecl_comma_list_opt) in
    Obj.repr(
# 47 "parser.mly"
  ( Fundecl(_1, _2, _4) )
# 359 "parser.ml"
               : 'fundecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
   ( [] )
# 365 "parser.ml"
               : 'vardecl_comma_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_def) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'vardecl_comma_list_opt) in
    Obj.repr(
# 55 "parser.mly"
 (Vardecl(_1,_2)::_4)
# 374 "parser.ml"
               : 'vardecl_comma_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lang.com) in
    Obj.repr(
# 59 "parser.mly"
              (_1)
# 381 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Lang.com) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Lang.com) in
    Obj.repr(
# 60 "parser.mly"
                                  (Seq(_1,_2))
# 389 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "parser.mly"
                (Skip)
# 395 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lang.com) in
    Obj.repr(
# 66 "parser.mly"
          (_2)
# 402 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Lang.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Lang.com) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Lang.com) in
    Obj.repr(
# 67 "parser.mly"
                                                                   (CondC(_3,_5, _6))
# 411 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lang.expr) in
    Obj.repr(
# 68 "parser.mly"
                           (Return(_2))
# 418 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                     (Exit)
# 424 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                        (Skip)
# 430 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Lang.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Lang.com) in
    Obj.repr(
# 72 "parser.mly"
    (Loop(Seq(CondC(_3, Skip, Exit),_5)))
# 438 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Lang.com) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Lang.expr) in
    Obj.repr(
# 75 "parser.mly"
    ( Loop(Seq(_2, CondC(_5, Skip, Exit))))
# 446 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Lang.expr) in
    Obj.repr(
# 76 "parser.mly"
                               (Assign(_1,_3))
# 454 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    Obj.repr(
# 77 "parser.mly"
                                           (CallC(_1,_3))
# 462 "parser.ml"
               : Lang.com))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 82 "parser.mly"
    ( Const (IntV _1) )
# 469 "parser.ml"
               : Lang.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Lang.expr) in
    Obj.repr(
# 84 "parser.mly"
    ( _2 )
# 476 "parser.ml"
               : Lang.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lang.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'binop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Lang.expr) in
    Obj.repr(
# 86 "parser.mly"
    ( BinOp (_2, _1, _3) )
# 485 "parser.ml"
               : Lang.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
    ( VarE(_1))
# 492 "parser.ml"
               : Lang.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Lang.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Lang.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Lang.expr) in
    Obj.repr(
# 90 "parser.mly"
    ( CondE (_1, _3, _5) )
# 501 "parser.ml"
               : Lang.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 92 "parser.mly"
    ( CallE(_1, _3) )
# 509 "parser.ml"
               : Lang.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
    ( [] )
# 515 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Lang.expr) in
    Obj.repr(
# 96 "parser.mly"
        ( [_1] )
# 522 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Lang.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 98 "parser.mly"
    ( _1 :: _3 )
# 530 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
            ( VoidT )
# 536 "parser.ml"
               : 'type_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
           ( IntT )
# 542 "parser.ml"
               : 'type_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
             ( FloatT )
# 548 "parser.ml"
               : 'type_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
              ( StringT )
# 554 "parser.ml"
               : 'type_def))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
           ( BArith( BAadd ))
# 560 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
           ( BArith(BAfadd) )
# 566 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
           ( BArith(BAsub) )
# 572 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
           ( BArith(BAfsub) )
# 578 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
           ( BArith( BAmul) )
# 584 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
           ( BArith(BAfmul) )
# 590 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
           ( BArith(BAdiv) )
# 596 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
           ( BArith(BAfdiv) )
# 602 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
           ( BArith(BAmod) )
# 608 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
         ( BCompar (BCeq))
# 614 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
         ( BCompar (BClt ))
# 620 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
         ( BCompar (BCle ))
# 626 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
         ( BCompar (BCgt ))
# 632 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
         ( BCompar(BCge ))
# 638 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
         ( BCompar(BCne))
# 644 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
          ( BBool(BBand) )
# 650 "parser.ml"
               : 'binop))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
         ( BBool(BBor) )
# 656 "parser.ml"
               : 'binop))
(* Entry start *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let start (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lang.prog)

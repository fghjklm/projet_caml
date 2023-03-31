%{
open Lang
%}

%token <string> IDENTIFIER
%token <string> LITCONSTANT
%token <string> STRINGCONSTANT
%token <float> FLOATCONSTANT
%token <bool> BOOLCONSTANT
%token <Lang.tp> TP
%token <int> INTCONSTANT
%token BANG NEG
%token PLUS MINUS TIMES DIV MOD PLUSF MINUSF TIMESF DIVF
%token LPAREN RPAREN LBRACE RBRACE
%token VOID_KW INT_KW FLOAT_KW STRING_KW
%token EQ COMMA SEMICOLON COLON QMARK
%token IF_KW ELSE_KW SWITCH_KW FOR_KW DO_KW WHILE_KW BREAK_KW CONTINUE_KW RETURN_KW
%token BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token EOF

%right ELSE
%left PLUS MINUS PLUSF MINUSF
%left TIMES DIV TIMESF DIVF
%left MOD


%start start
%type <Lang.prog> start

%type <Lang.com> block_item_list_opt
%type <Lang.com> block_item

%type <Lang.expr> exp
%%



start: fundefn {  Prog ([], [$1]) }
  ;

fundefn: 
  fundecl LBRACE block_item_list_opt RBRACE
  { Fundefn($1, $3) }
;

fundecl: TP IDENTIFIER LPAREN vardecl_comma_list_opt RPAREN 
  { Fundecl($1, $2, $4) }
;



vardecl_comma_list_opt:
  |{ [] }
  |type_def IDENTIFIER COMMA vardecl_comma_list_opt
 {Vardecl($1,$2)::$4}
;

block_item_list_opt:
  |block_item {$1}
  |block_item block_item_list_opt {Seq($1,$2)}
;

block_item:
  |/*  empty */ {Skip} 
  |LBRACE block_item_list_opt RBRACE
          {$2}
  |IF_KW LPAREN exp LPAREN block_item_list_opt block_item_list_opt {CondC($3,$5, $6)}
  |RETURN_KW exp SEMICOLON {Return($2)}
  |BREAK_KW SEMICOLON{Exit}
  |CONTINUE_KW SEMICOLON{Skip}
  | WHILE_KW LPAREN exp RPAREN block_item_list_opt
    {Loop(Seq(CondC($3, Skip, Exit),$5))}
  |DO_KW block_item_list_opt WHILE_KW
    LPAREN exp RPAREN SEMICOLON
    { Loop(Seq($2, CondC($5, Skip, Exit)))}
  |IDENTIFIER EQ exp SEMICOLON {Assign($1,$3)}
  |IDENTIFIER LPAREN args RPAREN SEMICOLON {CallC($1,$3)}
 ;

exp:
  |INTCONSTANT
    { Const (IntV $1) }
  | LPAREN exp RPAREN
    { $2 }
  | exp binop exp
    { BinOp ($2, $1, $3) }
  |IDENTIFIER
    { VarE($1)}
  | exp QMARK exp COLON exp
    { CondE ($1, $3, $5) }
  | IDENTIFIER LPAREN  args RPAREN
    { CallE($1, $3) }

args:
  | { [] }
  | exp { [$1] }
  | exp COMMA args
    { $1 :: $3 }

;

type_def:
  | VOID_KW { VoidT }
  | INT_KW { IntT }
  | FLOAT_KW { FloatT }
  | STRING_KW { StringT }
  ;

binop:
  | PLUS   { BArith( BAadd )}
  | PLUSF  { BArith(BAfadd) }
  | MINUS  { BArith(BAsub) }
  | MINUSF { BArith(BAfsub) }
  | TIMES  { BArith( BAmul) }
  | TIMESF { BArith(BAfmul) }
  | DIV    { BArith(BAdiv) }
  | DIVF   { BArith(BAfdiv) }
  | MOD    { BArith(BAmod) }
  | BCEQ { BCompar (BCeq)}
  | BCLT { BCompar (BClt )}
  | BCLE { BCompar (BCle )}
  | BCGT { BCompar (BCgt )}
  | BCGE { BCompar(BCge )}
  | BCNE { BCompar(BCne)}
  | BLAND { BBool(BBand) }
  | BLOR { BBool(BBor) }
;
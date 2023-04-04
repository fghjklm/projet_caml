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
%token LPAREN RPAREN 
%token LBRACE RBRACE
%token EQ COMMA SEMICOLON COLON QMARK
%token IF_KW ELSE_KW DO_KW WHILE_KW BREAK_KW CONTINUE_KW RETURN_KW
%token BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token EOF
%token FUNDECLDEF

%left COMMA
%right EQ 
%right QMARK COLON
%left BLOR
%left BLAND
%left BCEQ BCNE 
%left BCLE BCLT BCGE BCGT
%left PLUS MINUS PLUSF MINUSF
%left TIMES DIV  TIMESF DIVF MOD


%start start
%type <Lang.prog> start

%type <Lang.com> block_item_list_opt
%type <Lang.com> block_item

%type <Lang.expr> exp
%%

/*
start: fundecl_list fundefn_list{  Prog ($1, $2) }
  ;
*/

start:  fundecl_list FUNDECLDEF fundefn_list  EOF{ Prog($1,$3)};
|fundefn_list  EOF{ Prog([],$1)};

fundecl_list:
  /* empty */ {[]}
  | fundecl SEMICOLON fundecl_list  {$1:: $3}
;

fundefn_list:
  |/* empty */ {[]}
  |fundefn fundefn_list {$1 :: $2}
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
  |TP IDENTIFIER {[Vardecl($1,$2)]}
  |TP IDENTIFIER COMMA vardecl_comma_list_opt
 {Vardecl($1,$2)::$4}
;

block_item_list_opt:
  |block_item{$1}
  |block_item block_item_list_opt {Seq($1,$2)}
;

block_item:
  |IF_KW LPAREN exp RPAREN LBRACE block_item_list_opt RBRACE if_fstat
   {CondC($3,$6, $8)}
  |RETURN_KW exp SEMICOLON {Return($2)}
  |BREAK_KW SEMICOLON{Exit}
  |CONTINUE_KW SEMICOLON{Skip}
  | WHILE_KW LPAREN exp RPAREN LBRACE block_item_list_opt RBRACE
    {Loop(Seq(CondC($3, Skip, Exit),$6))}
  |DO_KW LBRACE block_item_list_opt RBRACE WHILE_KW
    LPAREN exp RPAREN SEMICOLON
    { Loop(Seq($3, CondC($7, Skip, Exit)))}
  |IDENTIFIER EQ exp SEMICOLON {Assign($1,$3)}
  |IDENTIFIER LPAREN args RPAREN SEMICOLON {CallC($1,$3)}
 ;
if_fstat:
  | { Skip }
  | ELSE_KW LBRACE block_item_list_opt  RBRACE{$3 }
exp:
  |INTCONSTANT
    { Const (IntV $1) }
  |FLOATCONSTANT   { Const (FloatV $1) }
  |STRINGCONSTANT { Const (StringV $1) }
  |BOOLCONSTANT { Const (BoolV $1) }

  | LPAREN exp RPAREN
    { $2 }
  | exp PLUS exp   { BinOp(BArith( BAadd ),$1,$3)}
  | exp PLUSF exp  { BinOp(BArith(BAfadd),$1,$3) }
  | exp MINUS exp { BinOp(BArith(BAsub),$1,$3)  }
  | exp MINUSF exp{ BinOp(BArith(BAfsub),$1,$3)  }
  | exp TIMES  exp { BinOp(BArith( BAmul),$1,$3)  }
  | exp TIMESF exp { BinOp(BArith(BAfmul),$1,$3)  }
  | exp DIV exp    { BinOp(BArith(BAdiv),$1,$3)  }
  | exp DIVF exp   { BinOp(BArith(BAfdiv),$1,$3)  }
  | exp MOD exp    { BinOp(BArith(BAmod),$1,$3)  }
  | exp BCEQ exp { BinOp(BCompar(BCeq),$1,$3) }
  | exp BCLT exp { BinOp(BCompar(BClt ),$1,$3) }
  | exp BCLE exp { BinOp(BCompar(BCle ),$1,$3) }
  | exp BCGT exp { BinOp(BCompar(BCgt ),$1,$3) }
  | exp BCGE exp { BinOp(BCompar(BCge ),$1,$3) }
  | exp BCNE exp { BinOp(BCompar(BCne),$1,$3) }
  | exp BLAND exp { BinOp(BBool(BBand),$1,$3) }
  | exp BLOR exp {BinOp(BBool(BBor),$1,$3) }
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
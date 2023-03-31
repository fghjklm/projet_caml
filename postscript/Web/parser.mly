%{
open Lang
%}

%token <string> IDENTIFIER
%token <string> LITCONSTANT
%token <Lang.tp> TP
%token <int> INTCONSTANT
%token BANG 
%token PLUS MINUS TIMES DIV MOD 
%token LPAREN RPAREN LBRACE RBRACE
%token VOID_KW INT_KW FLOAT_KW STRING_KW
%token EQ COMMA SEMICOLON COLON QMARK
%token IF_KW ELSE_KW SWITCH_KW FOR_KW DO_KW WHILE_KW BREAK_KW CONTINUE_K RETURN_KW
%token BCEQ BCGE BCGT BCLE BCLT BCNE BLAND BLOR
%token EOF

%right ELSE
%left PLUS MINUS 
%left TIMES DIV 
%left MOD


%start start
%type <Lang.prog> start
%type <Lang.com> com
%type <Lang.expr> exp
%%

type_def:
  | VOID_KW { VoidT }
  | INT_KW { IntT }
  | FLOAT_KW { FloatT }
  | STRING_KW { StringT }

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
  { [] }
  |t = type_def id = option(ID) COMMA vv = vardecl_comma_list_opt
 {Vardecl(t,id)::$3}
;

block_item_list_opt:
  |LBRACE sts = statement RBRACE
  { sts}
;

statement:
  |com {$1}
  |com statement {Seq($1,$2)}
;
com:
  IF_KW LPAREN cond = exp LPAREN tstat = statement fstat = if_fstat
  {Cond(cond,tstat, fstat)}(* Attention fstat de tpe Some exp ou None, ne pas se faire avoir!!*)
  |RETURN_KW e = exp SEMICOLON {Return(e)}
  |BREAK_KW {Exit}
  |CONTINUE_KW {Skip}
  | WHILE_KW LPAREN cond = exp RPAREN body = block_item_list_opt
    {Loop(Seq(CondC(cond, Skip, Exit),body))}
  | DO_KW body = block_item_list_opt WHILE_KW
    PAREN_OPEN cond = exp PAREN_CLOSE SEMICOLON
    { Loop(Seq(CondC(cond, Skip, Exit),body))}
  | FOR_KW PAREN_OPEN init = exp_assign SEMICOLON
    cond = exp SEMICOLON post = exp PAREN_CLOSE
    body = statement
    { Loop(Seq(CondC(cond, Skip, Exit),body)) }
  | ID EQ exp SEMICOLON {Assign($1, $3)}

;

exp:
  | i = INTCONSTANT
    { Const (IntV i) }
  | PAREN_OPEN e = exp PAREN_CLOSE
    { e }
  | e1 = exp op = binop e2 = exp
    { BinOp (op, e1, e2) }
  | BANG e = exp
    { UnOp (Not, e) }
  | MINUS e = exp %prec NEG_MINUS
    { UnOp (Negate, e) }
  | MULT e = exp %prec DEREF
    { Dereference e }
  | id = ID
    { VarE(id)}
  | cond = exp QUESTION texp = exp COLON fexp = expCondition
    { CondE (cond, texp, fexp) }
  | id = ID PAREN_OPEN args = args PAREN_CLOSE
    { CallE(id, args) }

args:
  | { [] }
  | e = exp { [e] }
  | e = exp COMMA es = args
    { e :: es }

;

if_fstat:
  | { None }
  | ELSE_KW fstat = statement { Some fstat }

%inline binop:
  | PLUS { Add }
  | MINUS { Sub }
  | MULT { Mult }
  | DIV { Div }
  | MOD { Mod }
  | BCEQ {BCompar (BCeq)}
  | BCLT {BCompar( BCLt )}
  | BCLE {BCompar (BCLe )}
  | BCGT {BCompar (BCGt )}
  | BCGE {BCompar(BCge )}
  | BCNE {BCompar(BCne)}
  | NEQ { Neq }
  | AND { BBand }
  | OR { BBor }
  | BIT_AND { BitAnd }
  | BIT_OR { BitOr }
  | XOR { Xor }
  | SHIFT_LEFT { ShiftL }
  | SHIFT_RIGHT { ShiftR }
;
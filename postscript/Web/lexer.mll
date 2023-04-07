{
  open Lexing
  open Parser
  open Lang
  exception Lexerror

  let pos lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let advance_line_pos pos =
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum; }

  let advance_line lexbuf =
    lexbuf.lex_curr_p <- advance_line_pos lexbuf.lex_curr_p
  let keyword_tabel =
    [("void", TP VoidT);
     ("int", TP IntT);
     ("float", TP FloatT);
     ("String", TP StringT);
     ("bool", TP BoolT);
     ("return", RETURN_KW);
     ("if", IF_KW);
     ("else", ELSE_KW);
     ("do", DO_KW);
     ("while", WHILE_KW);
     ("break", BREAK_KW);
     ("continue", CONTINUE_KW)]

let find_token s =
    match (List.assoc_opt s keyword_tabel )  with
    | Some kw -> kw
    | None -> IDENTIFIER s
}

let includeline = '#' [^ '\n']* '\n'
let alph =           ['a'-'z''A'-'Z']
let literal = '/'alph(alph|'-')*
let digit = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let string_constant = '"' [^'"'] '"'  
let float_constant = digit+"."(digit+)?


rule token = parse
 [' ' '\t']
    { token lexbuf }    (* white space: recursive call of lexer *)
|'\n'
    {advance_line lexbuf; token lexbuf }    (* white space: recursive call of lexer *)
| includeline
    { advance_line lexbuf; token lexbuf }    (* C include directives --> ignore *)
|"/* Function definition */" {FUNDECLDEF}
| "/*"
    { comment lexbuf }    (* comment --> ignore *)
| '('  { LPAREN }
| ')'  { RPAREN }
| '{'  { LBRACE }
| '}'  { RBRACE }
| ';' { SEMICOLON }
| ':' { COLON }
| ',' { COMMA }
| '?' { QMARK }
| '!' { BANG }

| '='  { EQ }

|"=="  { BCEQ }
| ">=" { BCGE } 
| ">"  { BCGT }
| "<=" { BCLE }
| "<"  { BCLT }
| "!=" { BCNE }

| "&&" {BLAND}
| "||" {BLOR}


| "+" { PLUS }
| "-" { MINUS }
| "*" { TIMES }
| "/" { DIV }

| "%" { MOD }
| "+." { PLUSF }
| "-." { MINUSF }
| "*." { TIMESF }
| "/." { DIVF }

| literal as l    { LITCONSTANT l }
|float_constant as f {FLOATCONSTANT(float_of_string f)}
| digit+ as i { INTCONSTANT (int_of_string i) }
|"true" {BOOLCONSTANT true}
|"false" {BOOLCONSTANT false} 
| '"' (([^'"'])* as s)  '"' { STRINGCONSTANT s } 
| id as id { find_token id }
|eof {EOF}
| _  {Printf.printf "ERROR: unrecogized symbol '%s'\n" (Lexing.lexeme lexbuf);
      raise Lexerror }

and
    ruleTail acc = parse
| eof { acc }
| _* as str { ruleTail (acc ^ str) lexbuf }

and comment = parse
| "*/" { token lexbuf }
| _ { comment lexbuf }
| eof { failwith "commentaire non termin e"}


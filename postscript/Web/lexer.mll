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
    [("void", VOID_KW);
     ("int", INT_KW);
     ("float", FLOAT_KW);
     ("String", STRING_KW)
     ("return", RETURN_KW);
     ("if", IF_KW);
     ("else", ELSE_KW);
     ("for", FOR_KW);
     ("do", DO_KW);
     ("while", WHILE_KW);
     ("break", BREAK_KW);
     ("continue", CONTINUE_KW)]

let find_token s =
    match List.Assoc.find keyword_tabel s ~equal:String.equal with
    | Some kw -> kw
    | None -> IDENTIFIER s
}

let includeline = '#' [^ '\n']* '\n'
let alph =           ['a'-'z''A'-'Z']
let literal = '/'alph(alph|'-')*
let comment = '/' '*' '*' '/'
let digit = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule token = parse
 [' ' '\t']
    { token lexbuf }    (* white space: recursive call of lexer *)
|'\n'
    {advance_line lexbuf; token lexbuf }    (* white space: recursive call of lexer *)
| includeline
    { advance_line lexbuf; token lexbuf }    (* C include directives --> ignore *)
| comment
    { token lexbuf }    (* comment --> ignore *)
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
| "<=" { BCCLE }
| "<"  { BCCLT }
| "!=" { BCNE }

| "&&" {BLAND}
| "||" {BLOR}


| "+" { PLUS }
| "-" { MINUS }
| "*" { MUL }
| "/" { TIMES }
| "%" { MOD }

| eof          {EOF}
| literal as l    { LITCONSTANT l }
| digit+ as i { INTCONSTANT (int_of_string i) }
| id as id { find_token id }

| _  {Printf.printf "ERROR: unrecogized symbol '%s'\n" (Lexing.lexeme lexbuf);
      raise Lexerror }

and
    ruleTail acc = parse
| eof { acc }
| _* as str { ruleTail (acc ^ str) lexbuf }

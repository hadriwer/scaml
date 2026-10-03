{
open Parser

exception Lex_error of string
}

let digit = ['0'-'9']
let int = digit+
let float = digit+ ['.'] digit*
let alpha = ['a'-'z' 'A'-'Z' '_' '\'']
let alnum = alpha | digit
let ident = alpha alnum*
let qualified_ident = ident ('.' ident)+
let symbols = ['+' '-' '*' '/' '<' '>' ':' '%' '.' '|' '=']
let custom_sym = symbols+
let whitespace = [' ' '\t' '\r']
let newline = '\n'

rule token = parse
  | whitespace+ { token lexbuf }
  | "//" [^ '\n']* { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | int as n { INT (int_of_string n) }
  | float as f { FLOAT f } (* Ast helper take a string and not a float *)
  | '\'' (['a'-'z' 'A'-'Z'] as c) '\'' { CHAR c }
  | "->" { ARROW }
  | "=" { EQ }
  | custom_sym as c { CUSTOM c }
  | "#use" { USE }
  | "fn"  { FN }
  | "trait" { TRAIT }
  | "type"  { TYPE }
  | "impl"  { IMPL }
  | "of"  { OF }
  | "op"  { OP }
  | "let" { LET }
  (* | "in" { IN } *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "^" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | qualified_ident as q { QIDENT q }
  | ident as id { IDENT id }
  | eof { EOF }
  | _ as c { raise (Lex_error (Printf.sprintf "Unexpected character: %c" c)) }

and read_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | eof { raise (Lex_error "Unterminated string literal") }

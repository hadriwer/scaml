{
open Parser

exception Lex_error of string

(* An operator's token carries its precedence, decided by its leading
   characters as in OCaml (so a user-defined operator gets the same
   priority as there, e.g. `*.` like `*`). *)
let operator c =
  if String.length c >= 2 && String.sub c 0 2 = "**" then OP_POW c
  else
    match c.[0] with
    | '*' | '/' | '%' -> OP_MUL c
    | '+' | '-' -> OP_ADD c
    | ':' when c = "::" -> OP_CONS c
    | '@' -> OP_AT c
    | '&' when c = "&" || c = "&&" -> OP_AND c
    | '|' when c = "||" -> OP_OR c
    | _ -> OP_CMP c
}

let digit = ['0'-'9']
let int = digit+
let float = digit+ ['.'] digit*
let alpha = ['a'-'z' 'A'-'Z' '_' '\'']
let alnum = alpha | digit
let ident = alpha alnum*
let uindent = ['A'-'Z'] alnum*
let qualified_ident = (uindent '.')+ ident
let symbols = ['+' '-' '*' '/' '<' '>' ':' '%' '.' '|' '=' '&' '!' '@']
let custom_sym = symbols+
let whitespace = [' ' '\t' '\r']
let newline = '\n'

rule raw_token = parse
  | whitespace+ { raw_token lexbuf }
  | "//" [^ '\n']* { raw_token lexbuf }
  | newline { Lexing.new_line lexbuf; raw_token lexbuf }
  | int as n { INT (int_of_string n) }
  (* `0..2`: longer than the float `0.`, so this wins; the `..` is then put
     back to be lexed as the next token (an operator), leaving just `0`. *)
  | (int as n) ".."
      { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 2;
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = lexbuf.lex_curr_p.pos_cnum - 2 };
        INT (int_of_string n) }
  | float as f { FLOAT f } (* Ast helper take a string and not a float *)
  | '\'' (['a'-'z' 'A'-'Z'] as c) '\'' { CHAR c }
  | "->" { ARROW }
  | "." { DOT }
  | "=" { EQ }
  | "|" { BAR } (* alone only: `||`, `|>` stay operators (longest match) *)
  | "type" { TYPE }
  | "#use" { USE }
  | "fn"  { FN }
  | "trait" { TRAIT }
  | "impl"  { IMPL }
  | "of"  { OF }
  | "op"  { OP }
  | "let" { LET }
  (* | "in" { IN } *)
  | "match" { MATCH }
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
  | "[|" { LBRACKBAR } (* before `[` and the `||` operator, so `[||]` is an empty array *)
  | "|]" { BARRBRACK }
  | "[" { LBRACK } (* or INDEX_LBRACK: see [token] at the end *)
  | "]" { RBRACK }
  | "," { COMMA }
  | ";" { SEMICOLON }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | qualified_ident as q { QIDENT q }
  | ident as id { IDENT id }
  | custom_sym as c { operator c }
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

{
(* `a[i]` (indexing) vs `f [1; 2]` (a list passed to `f`): a `[` glued to
   the end of an expression -- right after an identifier, `)`, `]` or `|]`, with
   no space in between -- opens an index; anywhere else it opens a literal.
   Decided from the previous token and where it ended, not by peeking at
   the lexer's buffer (whose previous byte may be gone after a refill). *)
let last : (Lexing.lexbuf * Parser.token * int) option ref = ref None

let token lexbuf =
  let tok = raw_token lexbuf in
  let start = (Lexing.lexeme_start_p lexbuf).pos_cnum in
  let tok =
    match tok, !last with
    | LBRACK, Some (lb, (IDENT _ | QIDENT _ | RPAREN | RBRACK | BARRBRACK), end_cnum) when lb == lexbuf && end_cnum = start -> INDEX_LBRACK
    | _ -> tok
  in
  last := Some (lexbuf, tok, (Lexing.lexeme_end_p lexbuf).pos_cnum);
  tok
}

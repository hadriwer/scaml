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
(* `1.5`, `1.`, and with an exponent, as in OCaml: `1e-6`, `2.5E3`. *)
let exponent = ['e' 'E'] ['+' '-']? digit+
let float = digit+ ['.'] digit* exponent? | digit+ exponent
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
  (* `'a'`, `' '`, `'1'`, or an escape as in OCaml: `'\n'`, `'\''`... Before
     [ident], which `'a'` (3 characters too) would also match. *)
  | '\'' ([^ '\\' '\'' '\n'] as c) '\'' { CHAR c }
  | '\'' '\\' (['\\' '\'' '"' 'n' 't' 'r' 'b' ' '] as c) '\''
      { CHAR (match c with 'n' -> '\n' | 't' -> '\t' | 'r' -> '\r' | 'b' -> '\b' | c -> c) }
  | "->" { ARROW }
  | "." { DOT }
  | "=" { EQ }
  | "?" { QUESTION }
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
  | "function" { FUNCTION }
  | "try" { TRY }
  | "with" { WITH }
  | "exception" { EXCEPTION }
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

(* Per lexbuf, the brackets still open, innermost first: [true] for an
   index's (`a[`), [false] for a list's (`[`). *)
let open_brackets : (Lexing.lexbuf * bool list ref) list ref = ref []

let brackets_of lexbuf =
  match List.find_opt (fun (lb, _) -> lb == lexbuf) !open_brackets with
  | Some (_, s) -> s
  | None -> let s = ref [] in open_brackets := (lexbuf, s) :: !open_brackets; s

(* Whether the last token was a `]` closing an index (`a[i]`). *)
let after_index = ref false

let operator_symbol = function
  | OP_OR s | OP_AND s | OP_CMP s | OP_AT s | OP_CONS s | OP_ADD s | OP_MUL s | OP_POW s -> Some s
  | _ -> None

let token lexbuf =
  let tok = raw_token lexbuf in
  let start = (Lexing.lexeme_start_p lexbuf).pos_cnum in
  let brackets = brackets_of lexbuf in
  let same_buf = match !last with Some (lb, _, _) -> lb == lexbuf | None -> false in
  let tok =
    match tok, !last with
    | LBRACK, Some (lb, (IDENT _ | QIDENT _ | RPAREN | RBRACK | BARRBRACK), end_cnum) when lb == lexbuf && end_cnum = start -> INDEX_LBRACK
    (* `a[i] += v`: an operator right after an index's `]` is the rest of
       an indexing operator, `[]+=`, when one is declared (otherwise it's
       the binary operator: `a[i] + v`). *)
    | _ when same_buf && !after_index ->
      (match operator_symbol tok with
       | Some c when Op_names.is_declared ("[]" ^ c) -> INDEX_OP c
       | _ -> tok)
    | _ -> tok
  in
  (match tok with
   | INDEX_LBRACK -> brackets := true :: !brackets
   | LBRACK -> brackets := false :: !brackets
   | _ -> ());
  after_index :=
    (match tok with
     | RBRACK -> (match !brackets with b :: rest -> brackets := rest; b | [] -> false)
     | _ -> false);
  last := Some (lexbuf, tok, (Lexing.lexeme_end_p lexbuf).pos_cnum);
  tok
}

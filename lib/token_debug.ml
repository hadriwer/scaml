let string_of_token : Parser.token -> string = function
  | Parser.CUSTOM s -> Printf.sprintf "CUSTOM %s" s
  | Parser.INT n -> Printf.sprintf "INT %d" n
  | Parser.IDENT s -> Printf.sprintf "IDENT %S" s
  | Parser.STRING s -> Printf.sprintf "STRING %S" s
  | Parser.EQ -> "EQ"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.LBRACE -> "LBRACE"
  | Parser.RBRACE -> "RBRACE"
  | Parser.COMMA -> "COMMA"
  | Parser.SEMICOLON -> "SEMICOLON"
  | Parser.LET -> "LET"
  | Parser.IF -> "IF"
  | Parser.THEN -> "THEN"
  | Parser.ELSE -> "ELSE"
  | Parser.FUN -> "FUN"
  | Parser.ARROW -> "ARROW"
  | Parser.TRUE -> "TRUE"
  | Parser.FALSE -> "FALSE"
  | Parser.FN -> "FN"
  | Parser.OP -> "OP"
  | Parser.USE -> "USE"
  | Parser.TRAIT -> "TRAIT"
  | Parser.IMPL -> "IMPL"
  | Parser.TYPE -> "TYPE"
  | Parser.OF -> "OF"
  | Parser.EOF -> "EOF"
  | Parser.FLOAT f -> Printf.sprintf "FLOAT %s" f
  | Parser.CHAR c -> Printf.sprintf "CHAR %c" c
  | Parser.QIDENT q -> Printf.sprintf "QIDENT %S" q
  | Parser.RBRACK -> Printf.sprintf "RBRACK"
  | Parser.LBRACK -> Printf.sprintf "LBRACK"

(* Repeatedly calls the lexer and prints each token with its source
   position, until EOF (inclusive). Used by the `--tokens` debug mode. *)
let print_tokens (lexbuf : Lexing.lexbuf) =
  let rec loop () =
    let tok = Lexer.token lexbuf in
    let pos = lexbuf.Lexing.lex_start_p in
    Printf.printf "%4d:%-3d  %s\n%!"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      (string_of_token tok);
    match tok with
    | Parser.EOF -> ()
    | _ -> loop ()
  in
  loop ()

(* Re-lexes [filename] from scratch and prints every token successfully
   produced, stopping silently if the lexer crashes (Lex_error). Used to
   show the tokens seen right before a lexing failure. *)
let dump_until_crash filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  (try print_tokens lexbuf with Lexer.Lex_error _ -> ());
  close_in ic

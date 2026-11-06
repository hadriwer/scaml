(* Resolver for `#use "path"` (lib/parser.mly), checked before the
   filesystem: bin/main.ml points it at the stdlib files built into the
   compiler, so the stdlib's own `#use "stdlib/..."` doesn't depend on the
   cwd. *)
let file : (string -> string option) ref = ref (fun _ -> None)

(* Parser for a `#use`d `.scaml` file ([filename], [contents]). Set by
   bin/main.ml, since the parser can't call its own entry point from inside
   a semantic action. *)
let parse_scaml : (string -> string -> Parsetree.structure) ref =
  ref (fun _ _ -> failwith "Embedded.parse_scaml not set")

(* The text of each file parsed, by name, for what a construct shows of its
   own source (`#time e;` prints `e` as written). Filled by bin/main.ml. *)
let sources : (string, string) Hashtbl.t = Hashtbl.create 8

(* The source text between two positions of a parsed file, on one line. *)
let source_text (s : Lexing.position) (e : Lexing.position) =
  match Hashtbl.find_opt sources s.pos_fname with
  | Some text when s.pos_cnum >= 0 && e.pos_cnum <= String.length text && s.pos_cnum <= e.pos_cnum ->
    let t = String.sub text s.pos_cnum (e.pos_cnum - s.pos_cnum) in
    Some (String.concat " " (List.filter (( <> ) "") (String.split_on_char ' ' (String.map (function '\n' | '\t' | '\r' -> ' ' | c -> c) t))))
  | _ -> None

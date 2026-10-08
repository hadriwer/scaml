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

(* Resolver for `#use "path"` (lib/parser.mly), checked before the
   filesystem: bin/main.ml points it at the stdlib files built into the
   compiler, so the stdlib's own `#use "stdlib/..."` doesn't depend on the
   cwd. *)
let file : (string -> string option) ref = ref (fun _ -> None)

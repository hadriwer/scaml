(* A type an impl targets, as one name: `int`, `btree`, or a type from an
   OCaml module, `Hashtbl.t`. The impl machinery builds identifiers from it
   (`Showable__hashtbl` modules, `print__ovl__...` overloads), so a
   qualified one is encoded with `'` in place of its dots: `Hashtbl't`.
   It stays a valid OCaml identifier, never contains `__` (the separator
   of those names), and can't collide with a type the program declares,
   whose name is lowercase. *)

let encode (qualified : string) = String.concat "'" (String.split_on_char '.' qualified)

(* `_` among an overload's types (`impl indexable of Hashtbl.t, _`): any
   type. Encoded the same way, as a name piece that is neither a type nor
   contains `__`. *)
let wildcard = "'"

let encode_impl_type ty = if ty = "_" then wildcard else ty

let is_qualified name = name <> "" && name.[0] >= 'A' && name.[0] <= 'Z' && String.contains name '\''

(* The type itself, to build a type expression from [name]. *)
let lid name =
  if is_qualified name then
    match Longident.unflatten (String.split_on_char '\'' name) with
    | Some l -> l
    | None -> Longident.Lident name
  else Longident.Lident name

(* A module type's name as the type-checker sees it (`Stdlib.Hashtbl.t`,
   `Stdlib__Hashtbl.t`), encoded as written in SCaml: `Hashtbl't`. *)
let of_path (path : Path.t) =
  let name = Path.name path in
  let strip prefix s =
    let n = String.length prefix in
    if String.length s > n && String.sub s 0 n = prefix then String.sub s n (String.length s - n) else s
  in
  encode (strip "Stdlib__" (strip "Stdlib." name))

(* How many parameters a module's type takes (2 for `Hashtbl.t`), read
   from OCaml's own environment: the parser can't know it otherwise. *)
let initial_env = lazy (Compmisc.init_path (); Compmisc.initial_env ())

let arity name =
  match Env.find_type_by_name (lid name) (Lazy.force initial_env) with
  | _, decl -> Some decl.type_arity
  | exception Not_found -> None

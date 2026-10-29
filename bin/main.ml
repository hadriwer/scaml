(* Pipeline: SCaml source
   -> Parsetree.structure (our lexer/parser, using OCaml's own AST)
   -> Typedtree.structure (OCaml's own type-checker, via compiler-libs)
   -> OCaml source text (OCaml's own pretty-printer, Pprintast)
   -> a real executable (compiled by ocamlfind/ocamlopt). *)

let loc = Location.none

(* The tiny standard library SCaml programs get for free. *)
(* let prelude : Parsetree.structure =
  [ Ast_helper.Str.value ~loc Asttypes.Nonrecursive
      [ Ast_helper.Vb.mk ~loc
          (Ast_helper.Pat.var ~loc (Location.mkloc "print" loc))
          (Ast_helper.Exp.ident ~loc
             (Location.mkloc (Longident.Lident "print_endline") loc)) ]
  ] *)

let has_main (structure : Parsetree.structure) =
  List.exists
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_value (_, vbs) ->
        List.exists
          (fun (vb : Parsetree.value_binding) ->
            match vb.pvb_pat.ppat_desc with
            | Ppat_var { txt = "main"; _ } -> true
            | _ -> false)
          vbs
      | _ -> false)
    structure

(* The top-level names a structure_item defines (its "address" -- what a
   reference elsewhere would name to depend on it). *)
let provided_names (item : Parsetree.structure_item) : string list =
  match item.pstr_desc with
  | Pstr_value (_, vbs) ->
    (* Every variable of each pattern: `let (w, h) = ..;` provides both. *)
    let names = ref [] in
    let pat self (p : Parsetree.pattern) =
      (match p.ppat_desc with Ppat_var { txt; _ } | Ppat_alias (_, { txt; _ }) -> names := txt :: !names | _ -> ());
      Ast_iterator.default_iterator.pat self p
    in
    let it = { Ast_iterator.default_iterator with pat } in
    List.iter (fun (vb : Parsetree.value_binding) -> it.pat it vb.pvb_pat) vbs;
    List.rev !names
  | Pstr_module mb -> (match mb.pmb_name.txt with Some n -> [ n ] | None -> [])
  | Pstr_modtype mtd -> [ mtd.pmtd_name.txt ]
  | Pstr_type (_, decls) ->
    (* A type is also needed when only its record fields or constructors are
       used (`l.name`, `{ name = ... }`), never its name: those are provided
       too, prefixed so they can't collide with values of the same name. *)
    List.concat_map
      (fun (d : Parsetree.type_declaration) ->
        d.ptype_name.txt
        :: (match d.ptype_kind with
            | Ptype_record lds -> List.map (fun (ld : Parsetree.label_declaration) -> "." ^ ld.pld_name.txt) lds
            | Ptype_variant cds -> List.map (fun (cd : Parsetree.constructor_declaration) -> "#" ^ cd.pcd_name.txt) cds
            | _ -> []))
      decls
  | _ -> []

(* Every reference a structure_item's own definition makes -- walking
   expressions (`Pexp_ident`), module expressions (`Pmod_ident`, e.g.
   `Iterable__array` in a functor application), module types (`Pmty_ident`,
   e.g. `arithm` in a `: trait_name` ascription) and type expressions
   (`Ptyp_constr`, e.g. `array`, or `a` in a `with type` manifest) anywhere
   inside it. A plain name (`Lident n`) gives `(n, None)`; a qualified one
   (`Ldot(Lident m, x)`, e.g. `Arithm__int.op___0___`) gives `(m, Some x)`,
   so callers can tell "depends on the module" from "depends on specifically
   this member of it". Names matching nothing we track (OCaml's own stdlib,
   `int`, ...) are harmless noise, simply never found in a lookup table. *)
let referenced (item : Parsetree.structure_item) : (string * string option) list =
  let found = ref [] in
  let add_lid (lid : Longident.t) =
    match lid with
    | Longident.Lident n -> found := (n, None) :: !found
    | Longident.Ldot (m, x) ->
      (match Longident.flatten m.Location.txt with
       | head :: _ -> found := (head, Some x.Location.txt) :: !found
       | [] -> ())
    | Longident.Lapply _ -> ()
  in
  (* A record field or constructor: a plain one depends on its type's
     declaration (see [provided_names]), a qualified one on its module. *)
  let add_member prefix (lid : Longident.t) =
    match lid with
    | Longident.Lident n -> found := (prefix ^ n, None) :: !found
    | _ -> add_lid lid
  in
  let expr self (e : Parsetree.expression) =
    (match e.pexp_desc with
     | Pexp_ident { txt; _ } -> add_lid txt
     | Pexp_field (_, { txt; _ }) | Pexp_setfield (_, { txt; _ }, _) -> add_member "." txt
     | Pexp_record (fields, _) -> List.iter (fun ({ Location.txt; _ }, _) -> add_member "." txt) fields
     | Pexp_construct ({ txt; _ }, _) -> add_member "#" txt
     | _ -> ());
    Ast_iterator.default_iterator.expr self e
  in
  let pat self (p : Parsetree.pattern) =
    (match p.ppat_desc with
     | Ppat_record (fields, _) -> List.iter (fun ({ Location.txt; _ }, _) -> add_member "." txt) fields
     | Ppat_construct ({ txt; _ }, _) -> add_member "#" txt
     | _ -> ());
    Ast_iterator.default_iterator.pat self p
  in
  let module_expr self (me : Parsetree.module_expr) =
    (match me.pmod_desc with Pmod_ident { txt; _ } -> add_lid txt | _ -> ());
    Ast_iterator.default_iterator.module_expr self me
  in
  let module_type self (mt : Parsetree.module_type) =
    (match mt.pmty_desc with Pmty_ident { txt; _ } -> add_lid txt | _ -> ());
    Ast_iterator.default_iterator.module_type self mt
  in
  let typ self (t : Parsetree.core_type) =
    (match t.ptyp_desc with Ptyp_constr ({ txt; _ }, _) -> add_lid txt | _ -> ());
    Ast_iterator.default_iterator.typ self t
  in
  let iterator = { Ast_iterator.default_iterator with expr; pat; module_expr; module_type; typ } in
  iterator.structure_item iterator item;
  !found

(* Reachability over a flat list of structure_items (used both at the top
   level and, recursively, inside a kept module's own body): keeps whatever
   is transitively needed starting from the items with no name at all (plain
   top-level expressions -- in particular the entry point, `Pstr_eval (main
   ())`) plus an explicit extra set of root names (used to seed "this
   module's externally-used members" when pruning inside a module). Returns
   the kept items (original order preserved) together with, per item, the
   (module, member) pairs it referenced -- so a caller can recurse into any
   kept module using exactly the members its own keepers actually needed. *)
let reachable (extra_roots : string list) (items : Parsetree.structure_item list) =
  let indexed = List.mapi (fun i item -> (i, item, provided_names item, referenced item)) items in
  (* Several items may provide the same name (two types both with a `One`
     constructor, a value shadowing another...): a reference keeps them all,
     since telling which one it means would take OCaml's own scoping and
     type-directed disambiguation. Keeping one too many is harmless. *)
  let provider_of_name : (string, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i, _, provided, _) -> List.iter (fun n -> Hashtbl.add provider_of_name n i) provided) indexed;
  let kept = Hashtbl.create 64 in
  let rec visit i =
    if not (Hashtbl.mem kept i) then begin
      Hashtbl.add kept i ();
      let (_, _, _, refs) = List.nth indexed i in
      List.iter (fun (n, _) -> List.iter visit (Hashtbl.find_all provider_of_name n)) refs
    end
  in
  List.iter (fun (i, _, provided, _) -> if provided = [] then visit i) indexed;
  List.iter (fun n -> List.iter visit (Hashtbl.find_all provider_of_name n)) extra_roots;
  List.filter_map (fun (i, item, _, refs) -> if Hashtbl.mem kept i then Some (item, refs) else None) indexed

(* A kept module's body, pruned to just the members [needed] (collected from
   every *other* kept item's references to it) plus anything those members
   themselves need in turn. Any ascription is dropped in the process: it
   already did its one-time job (verifying, in the full, unpruned structure,
   that this impl truly satisfies its trait -- see [item]'s IMPL rule in
   lib/parser.mly) and would otherwise reject the very members we just
   removed ("val op___0___ is required but not provided"). The abstract
   types' own manifests (`type a = int`, `type b = a array`, ...), which are
   what actually keeps them transparent, live in the structure itself and
   are untouched. If nothing is known to be needed (e.g. the module is used
   some other way we don't track), the module is left exactly as-is rather
   than risk pruning something still required. *)
let rec prune_module_expr (needed : string list) (me : Parsetree.module_expr) : Parsetree.module_expr =
  match me.pmod_desc with
  | _ when needed = [] -> me
  | Pmod_constraint (inner, _) -> prune_module_expr needed inner
  | Pmod_functor (param, body) -> { me with pmod_desc = Pmod_functor (param, prune_module_expr needed body) }
  | Pmod_structure items ->
    let kept = reachable needed items in
    { me with pmod_desc = Pmod_structure (List.map fst kept) }
  | _ -> me

(* Keeps only the structure_items transitively reachable from `main`
   (specifically: the entry point, `Pstr_eval (main ())`), then prunes each
   kept module down to the members actually used anywhere else in what's
   kept. Everything unreachable at either level (e.g. a whole unused
   `Arithm__float`, or just the unused `op___0___`/`op___3___`/`op___4___`
   inside an `Arithm__int` that IS used) is dropped -- the *whole* stdlib is
   spliced into *every* program by [load_stdlib], so most of it is normally
   dead code for any one given SCaml file. Relative order is preserved, so
   declare-before-use still holds among whatever remains. *)
let eliminate_dead_code (structure : Parsetree.structure) : Parsetree.structure =
  let kept = reachable [] structure in
  let members_of = Hashtbl.create 64 in
  (* A module referenced *bare* (e.g. passed whole as a functor argument,
     `Mod.ident "Arithm__int"` for `Sumable__array(Arithm__int)`) needs
     every one of its members, not just whichever specific ones happen to
     also be referenced elsewhere by qualified name (e.g. `Arithm__int.
     op___0___` used directly too) -- pruning down to just those would
     strip e.g. `type a`, breaking the very module ascription that bare
     reference relies on ("the type a is required but not provided"). *)
  let fully_needed = Hashtbl.create 16 in
  List.iter
    (fun (_, refs) ->
      List.iter
        (function
          | m, None -> Hashtbl.replace fully_needed m ()
          | m, Some member -> Hashtbl.replace members_of m (member :: (try Hashtbl.find members_of m with Not_found -> [])))
        refs)
    kept;
  List.map
    (fun ((item : Parsetree.structure_item), _) ->
      match item.pstr_desc with
      | Pstr_module mb ->
        let needed =
          match mb.pmb_name.txt with
          | Some n when Hashtbl.mem fully_needed n -> []
          | Some n -> (try Hashtbl.find members_of n with Not_found -> [])
          | None -> []
        in
        { item with pstr_desc = Pstr_module { mb with pmb_expr = prune_module_expr needed mb.pmb_expr } }
      | _ -> item)
    kept

(* The types known/concrete enough that a trait signature can name them
   directly (mirrors lib/parser.mly's [known_types]). Any other name in a
   signature is one of the trait's own abstract placeholders. *)
let known_type_names = [ "unit"; "int"; "float"; "string"; "bytes"; "bool"; "char" ]

(* When a trait method parameter is itself a function type with one of the
   trait's abstract placeholders somewhere in its own domain chain (e.g.
   `iter`'s first parameter `a -> unit`, or `iteri`'s `int -> a -> unit`,
   where `int` is a real argument -- the index -- and `a` is the one that
   actually matters here), [param_domains] records that placeholder name
   *and* how many arrow-levels deep it sits (0 for `iter`'s case, 1 for
   `iteri`'s) at that position (`None` everywhere else). The level lets
   [harvest_dispatch] find the right bound variable when a callback
   *value* is actually passed for that parameter (e.g. `iteri (fun i e ->
   ...)`: the relevant, dispatch-worthy parameter is `e`, the second one,
   not `i`) -- used to resolve a *bare* trait-method value passed as such a
   callback (e.g. `println` in `iter println arr`, which never appears as
   the head of its own application and so can't be found via
   [classify_texpr] on real argument expressions the way the outer call's
   own self type is), and to find a callback's own bound element parameter
   when scanning its body for further calls (see [scan_callback_body]). A
   domain naming one of the known concrete types (e.g. `iteri`'s `int`) is
   skipped over rather than treated as the placeholder of interest. *)
let fn_param_domain (pty : Parsetree.core_type) : (string * int) option =
  let rec go (pty : Parsetree.core_type) (level : int) =
    match pty.ptyp_desc with
    | Ptyp_arrow (_, dom, rest) ->
      (match dom.ptyp_desc with
       | Ptyp_constr ({ txt = Longident.Lident n; _ }, []) when not (List.mem n known_type_names) -> Some (n, level)
       | _ -> go rest (level + 1))
    | _ -> None
  in
  go pty 0

let rec param_domains (ty : Parsetree.core_type) : (string * int) option list =
  match ty.ptyp_desc with
  | Ptyp_arrow (_, t1, t2) -> fn_param_domain t1 :: param_domains t2
  | _ -> []

let collect_trait_methods (structure : Parsetree.structure) : (string * string * string list * (string * int) option list) list =
  (* The full params-then-return name chain, e.g. ["a"; "a"; "bool"] for
     `a -> a -> bool` (length = arity + 1, last = return). The *same* name
     appearing more than once (like `a` here) means the trait requires
     those positions to share a type -- which the probe stub must respect,
     see [mk_probe_stub]. *)
  let rec type_names (ty : Parsetree.core_type) : string list =
    match ty.ptyp_desc with
    | Ptyp_arrow (_, t1, t2) ->
      let n =
        match t1.ptyp_desc with
        | Ptyp_constr ({ txt = Longident.Lident n; _ }, []) -> n
        | _ -> "_"
      in
      n :: type_names t2
    | Ptyp_constr ({ txt = Longident.Lident n; _ }, []) -> [ n ]
    | _ -> [ "_" ]
  in
  List.concat_map
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_modtype { pmtd_name; pmtd_type = Some { pmty_desc = Pmty_signature sigs; _ }; _ } ->
        List.filter_map
          (fun (sig_item : Parsetree.signature_item) ->
            match sig_item.psig_desc with
            | Psig_value vd ->
              Some (vd.pval_name.txt, pmtd_name.txt, type_names vd.pval_type, param_domains vd.pval_type)
            | _ -> None)
          sigs
      | _ -> [])
    structure

(* Same module-name convention as `mkimpl` in lib/parser.mly: capitalize the
   whole `trait__type` string (only the first character needs to be
   uppercase for a valid OCaml module name). *)
let impl_module_name trait_name type_name =
  String.capitalize_ascii (trait_name ^ "__" ^ type_name)

(* Maps a Types.type_expr back to one of the concrete type names `trait`
   signatures can use (lib/parser.mly's [known_types]), plus -- for a
   parametric type like `array` -- the same classification recursively
   applied to each of its own type arguments, kept as a full tree (e.g.
   `int array array` gives `Ty ("array", [Ty ("array", [Ty ("int", [])])])`,
   so a nested container's innermost element type isn't lost). None if [ty] (or one of its arguments) isn't one
   of these recognized shapes, in which case that call site is left
   unresolved. [ty] is expanded ([Ctype.expand_head]) first: an impl
   method's own self parameter is now annotated with the impl's own type
   name (e.g. `(a : a)`, see [[lib/parser.mly]'s [annotate_self_params]]),
   and unexpanded that's just an opaque reference to "a" itself, not the
   concrete (or still-abstract-via-a-functor-parameter) shape it's really
   a manifest for. *)
type type_tree = Ty of string * type_tree list

let ty_name (Ty (n, _)) = n

(* The impl on a nested type (`impl conversionnable_matrix of e array
   array`, see lib/impl_pattern.ml) of [trait] for the type [tree], if one
   matches it: the most specific, its module and what its parameters stand
   for here, in order (`int` for `e`, for an `int array array`). Any such
   impl is more specific than the usual one on the outermost type alone
   (`impl .. of array`); two as specific as each other are ambiguous. *)
(* The expression being dispatched, for an error about its impls. *)
let dispatch_loc = ref Location.none

(* [bind pattern tree] gives what each of [pattern]'s parameters stands for
   in [tree], if it matches: over a [type_tree], or an [xtree] (see
   [find_impl_x]). *)
let find_impl_with (bind : SCaml.Impl_pattern.t -> 'a -> (string * 'a) list -> (string * 'a) list option) trait (tree : 'a) =
  let matching =
    List.filter_map
      (fun (impl : SCaml.Impl_pattern.impl) ->
        Option.map (fun bound -> (impl, bound)) (bind impl.pattern tree []))
      (SCaml.Impl_pattern.impls_of trait)
  in
  let spec ((impl : SCaml.Impl_pattern.impl), _) = SCaml.Impl_pattern.specificity impl.pattern in
  match List.sort (fun a b -> compare (spec b) (spec a)) matching with
  | [] -> None
  | ((best : SCaml.Impl_pattern.impl), bound) :: rest ->
    (match List.find_opt (fun m -> spec m = spec (best, bound)) rest with
     | Some ((other : SCaml.Impl_pattern.impl), _) ->
       raise (Location.Error (Location.error ~loc:!dispatch_loc
         (Printf.sprintf "ambiguous impls of trait `%s`: `%s` and `%s` both apply, and neither is more specific"
            trait (SCaml.Impl_pattern.to_string best.pattern) (SCaml.Impl_pattern.to_string other.pattern))))
     | None -> Some (best.module_name, List.map (fun p -> List.assoc p bound) best.params))

let find_impl trait (tree : type_tree) : (string * type_tree list) option =
  let rec bind (p : SCaml.Impl_pattern.t) (Ty (n, args) as t) acc =
    match p with
    | Var v -> Some ((v, t) :: acc)
    | Con (c, ps) when c = n && List.compare_lengths ps args = 0 ->
      List.fold_left2 (fun acc p t -> Option.bind acc (bind p t)) (Some acc) ps args
    | Con _ -> None
  in
  find_impl_with bind trait tree

(* The impl of [trait] for [Ty (n, args)] and the types its functor
   parameter's fields stand for: one on the nested type if any matches,
   else the usual one, named after [n], with [args]. *)
let impl_for trait (Ty (n, args) as tree) =
  match find_impl trait tree with
  | Some found -> found
  | None -> (impl_module_name trait n, args)

(* Top-level types the program itself declares (`type test_struct { ... }`),
   which an `impl ... of test_struct` can target just like a predefined one.
   Filled once from the full structure before dispatch starts. *)
let user_type_names : (string, unit) Hashtbl.t = Hashtbl.create 16

(* Types from OCaml modules that some impl targets (`impl showable of
   Hashtbl.t`), encoded (see lib/type_name.ml): found in the names of the
   impl modules and overloads generated for them. *)
let qualified_type_names : (string, unit) Hashtbl.t = Hashtbl.create 16

(* Each generic impl's functor parameter fields, in the order of its type's
   own parameters (`[ "__elem0__" ]` for `showable of array`, `[ "k"; "v"
   ]` for `iterable of (k, v) Hashtbl.t`): what a call site must give. *)
let impl_functor_params : (string, string list) Hashtbl.t = Hashtbl.create 16

(* An impl with several parameters, which can need a dictionary for each
   (see [upgrade_dict_impls]). *)
let is_multi_impl mod_name =
  match Hashtbl.find_opt impl_functor_params mod_name with
  | Some (_ :: _ :: _) -> true
  | _ -> false

(* ...and needs one for every parameter, as [reqs] (field, trait) says. *)
let multi_complete mod_name reqs =
  is_multi_impl mod_name
  && List.for_all (fun p -> List.mem_assoc p reqs) (Hashtbl.find impl_functor_params mod_name)

(* The functor parameter holding parameter [field]'s dictionary, in such an
   impl. *)
let multi_dict_param field = "X__" ^ field

let collect_user_type_names (structure : Parsetree.structure) =
  (* `Showable__Hashtbl't` -> [ "Showable"; "Hashtbl't" ] *)
  let rec parts s =
    let n = String.length s in
    let rec find i = if i + 1 >= n then None else if s.[i] = '_' && s.[i + 1] = '_' then Some i else find (i + 1) in
    match find 0 with
    | Some j -> String.sub s 0 j :: parts (String.sub s (j + 2) (n - j - 2))
    | None -> [ s ]
  in
  let add_qualified name =
    List.iter
      (fun part -> if SCaml.Type_name.is_qualified part then Hashtbl.replace qualified_type_names part ())
      (parts name)
  in
  List.iter
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_type (_, decls) ->
        List.iter (fun (d : Parsetree.type_declaration) -> Hashtbl.replace user_type_names d.ptype_name.txt ()) decls
      | Pstr_module { pmb_name = { txt = Some name; _ }; pmb_expr; _ } ->
        add_qualified name;
        (* an impl on a nested type: the module types of its pattern *)
        Hashtbl.iter
          (fun _ (impl : SCaml.Impl_pattern.impl) ->
            if impl.module_name = name then begin
              let rec go = function
                | SCaml.Impl_pattern.Var _ -> ()
                | Con (c, args) ->
                  if SCaml.Type_name.is_qualified c then Hashtbl.replace qualified_type_names c ();
                  List.iter go args
              in
              go impl.pattern
            end)
          SCaml.Impl_pattern.table;
        (match pmb_expr.pmod_desc with
         | Pmod_functor (Named (_, { pmty_desc = Pmty_signature sigs; _ }), _) ->
           Hashtbl.replace impl_functor_params name
             (List.concat_map
                (fun (si : Parsetree.signature_item) ->
                  match si.psig_desc with
                  | Psig_type (_, ds) -> List.map (fun (d : Parsetree.type_declaration) -> d.ptype_name.txt) ds
                  | _ -> [])
                sigs)
         | _ -> ())
      | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; _ } ]) -> add_qualified txt
      | _ -> ())
    structure

let known_type_name (path : Path.t) =
  match path with
  | Path.Pident id when Hashtbl.mem user_type_names (Ident.name id) -> Some (Ident.name id)
  | Path.Pdot _ when Hashtbl.mem qualified_type_names (SCaml.Type_name.of_path path) -> Some (SCaml.Type_name.of_path path)
  | _ ->
  if Path.same path Predef.path_int then Some "int"
  else if Path.same path Predef.path_string then Some "string"
  else if Path.same path Predef.path_bytes then Some "bytes"
  else if Path.same path Predef.path_float then Some "float"
  else if Path.same path Predef.path_bool then Some "bool"
  else if Path.same path Predef.path_char then Some "char"
  else if Path.same path Predef.path_unit then Some "unit"
  else if Path.same path Predef.path_array then Some "array"
  else if Path.same path Predef.path_list then Some "list"
  else if Path.same path Predef.path_option then Some "option"
  else None

(* Just the outermost constructor's name (`array` for `'a array`, whose
   element type may well still be unknown). *)
let head_type_name (env : Env.t) (ty : Types.type_expr) =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, _, _) -> known_type_name path
  | _ -> None

(* Tuples have no named constructor to write an `impl showable of ...`
   for, and an impl written in SCaml gets a single dictionary while a tuple
   needs one per component. So their `showable` is generated here instead,
   directly in OCaml, as functors taking one `showable` per component
   (`Showable__tuple2 (A : showable) (B : showable)`), inserted right after
   the stdlib's `showable` trait. A call on a tuple then dispatches to that
   functor applied to its components' impls (see [DictCall]). *)
let tuple_sizes = [ 2; 3; 4; 5; 6 ]

let tuple_impls : (string, unit) Hashtbl.t = Hashtbl.create 8

let tuple_showable_source n =
  let comps = List.init n (fun i -> i) in
  let params = String.concat " " (List.map (fun i -> Printf.sprintf "(M%d : showable)" i) comps) in
  let ty = String.concat " * " (List.map (fun i -> Printf.sprintf "M%d.a" i) comps) in
  let pat = String.concat ", " (List.map (fun i -> Printf.sprintf "x%d" i) comps) in
  let prints =
    String.concat "; scaml_print_string \", \"; "
      (List.map (fun i -> Printf.sprintf "M%d.print x%d" i i) comps)
  in
  Printf.sprintf
    "module Showable__tuple%d %s = struct\n\
    \  type a = %s\n\
    \  let print ((%s) : a) = scaml_print_string \"(\"; %s; scaml_print_string \")\"\n\
    \  let println (v : a) = print v; scaml_print_newline ()\n\
     end\n"
    n params ty pat prints

(* [stdlib] with the tuple functors inserted after its `showable` trait
   (unchanged if there's no such trait). *)
let with_tuple_impls (stdlib : Parsetree.structure) : Parsetree.structure =
  List.concat_map
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_modtype { pmtd_name = { txt = "showable"; _ }; _ } ->
        let generated =
          List.concat_map
            (fun n ->
              Hashtbl.replace tuple_impls (Printf.sprintf "Showable__tuple%d" n) ();
              Parse.implementation (Lexing.from_string (tuple_showable_source n)))
            tuple_sizes
        in
        item :: generated
      | _ -> [ item ])
    stdlib

(* Set only for a last-resort dispatch pass (see the end of the pipeline):
   a type variable nothing constrains (`'a` in `last []`'s `'a option`) is
   classified as `unit`. Sound since no value of such a type can exist (the
   option can only be `None`), but only tried once normal dispatch has left
   something unresolved, and kept only if the result type-checks. *)
let default_free_type_vars = ref false

(* Set, on top of [default_free_type_vars], only for a last retry when the
   `unit` reading still fails: a clone's free type variable is then read as
   an empty type, [never_type_name], instead (see [core_of_type]). No value of it can
   exist, so a trait call on one is dead code: `x + v` in the `Some x` arm
   of `fn foo ?u v`, called as `foo 2` (`u` is always `None` there), which
   has no impl for `unit, int`. Such a call becomes `assert false` (see
   [Absurd]) instead of an error. *)
let absurd_free_type_vars = ref false

(* The empty type, declared in front of the program for that retry only.
   (Not OCaml's `[ ]`: two of those never unify.) *)
let never_type_name = "scaml_never"

let never_type_decl =
  Ast_helper.Str.type_ ~loc Recursive [ Ast_helper.Type.mk ~loc ~kind:(Ptype_variant []) (Location.mkloc never_type_name loc) ]

let is_empty_type (env : Env.t) (ty : Types.type_expr) =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Tconstr (Pident id, [], _) -> Ident.name id = never_type_name
  | _ -> false

(* The last-resort pass failed on a trait call with no impl: the report,
   kept for when the [absurd_free_type_vars] retry fails too. *)
exception Overload_miss of Location.error

(* The type variables appearing in the type of some `let`/`fn` binding of
   the current probe (e.g. the parameter type of a generic `length`):
   those are genuinely polymorphic and must stay so, to be cloned per use.
   Filled by [protect_bound_vars] before each last-resort round. *)
let protected_vars : Types.type_expr list ref = ref []

let protect_bound_vars (typed : Typedtree.structure) =
  let found = ref [] in
  let value_binding it (vb : Typedtree.value_binding) =
    (* `let q = make_queue ()`: an application is not generalizable (value
       restriction), so its type variables are not polymorphic, just not
       yet known: they can be defaulted. *)
    (match vb.vb_expr.exp_desc with
     | Texp_apply _ -> ()
     | _ -> found := Ctype.free_variables vb.vb_pat.pat_type @ !found);
    Tast_iterator.default_iterator.value_binding it vb
  in
  let it = { Tast_iterator.default_iterator with value_binding } in
  it.structure it typed;
  protected_vars := !found

(* Only a variable that is free for real -- the `'a` of `length []` inside
   `main`, which appears in no binding's type -- never one that some
   binding is polymorphic in. (Levels can't tell them apart: OCaml
   generalizes every variable created inside a top-level definition.) *)
let defaultable (ty : Types.type_expr) =
  !default_free_type_vars && not (List.exists (Types.eq_type ty) !protected_vars)

let rec classify_texpr (env : Env.t) (ty : Types.type_expr) : type_tree option =
  let ty = Ctype.expand_head env ty in
  match Types.get_desc ty with
  | Tvar _ when defaultable ty -> Some (Ty ("unit", []))
  (* `a * b` reads as a two-parameter `tuple2` (see [tuple_impls]). *)
  | Ttuple components ->
    let classified = List.map (fun (_, t) -> classify_texpr env t) components in
    if List.for_all Option.is_some classified then
      Some (Ty (Printf.sprintf "tuple%d" (List.length components), List.map Option.get classified))
    else None
  | Tconstr (path, args, _) ->
    let name = known_type_name path in
    (match name with
     | None -> None
     | Some n ->
       let classified = List.map (classify_texpr env) args in
       if List.for_all Option.is_some classified then Some (Ty (n, List.map Option.get classified))
       else None)
  | _ -> None

(* An arrow's domain comes wrapped as a monomorphic [Tpoly (t, [])];
   unwrap it so [classify_texpr] sees [t]. *)
let strip_tpoly (ty : Types.type_expr) =
  match Types.get_desc ty with Tpoly (t, []) -> t | _ -> ty

(* A trait method whose trait declares a type constructor (`type t of 1`):
   its trait, that constructor's name and the method's full declared type
   (polymorphic in everything but the constructor, e.g. `('a -> 'b) -> 'a
   t -> 'b t`). *)
type ctor_method = {
  cm_trait : string;
  cm_ctor : string;
  cm_sig : Parsetree.core_type;
}

let collect_ctor_methods (structure : Parsetree.structure) : (string, ctor_method) Hashtbl.t =
  let table = Hashtbl.create 16 in
  List.iter
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_modtype { pmtd_name; pmtd_type = Some { pmty_desc = Pmty_signature sigs; _ }; _ } ->
        let decls =
          List.concat_map
            (fun (si : Parsetree.signature_item) -> match si.psig_desc with Psig_type (_, ds) -> ds | _ -> [])
            sigs
        in
        (match List.partition (fun (d : Parsetree.type_declaration) -> d.ptype_params <> []) decls with
         | [ ctor ], _ ->
           List.iter
             (fun (si : Parsetree.signature_item) ->
               match si.psig_desc with
               | Psig_value vd ->
                 Hashtbl.replace table vd.pval_name.txt
                   { cm_trait = pmtd_name.txt;
                     cm_ctor = ctor.ptype_name.txt;
                     cm_sig = vd.pval_type }
               | _ -> ())
             sigs
         | _ -> ())
      | _ -> ())
    structure;
  table

(* Per overloaded trait method (see lib/parser.mly's [mkoverloadimpl]):
   each overload's parameter type names and its generated function's name,
   recovered from that name (`<method>__ovl__<ty1>__<ty2>...`). *)
let collect_overloads (structure : Parsetree.structure) : (string, string list * string) Hashtbl.t =
  let table = Hashtbl.create 16 in
  let marker = "__ovl__" in
  let find_sub s sub =
    let n = String.length s and m = String.length sub in
    let rec go i = if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1) in
    go 0
  in
  List.iter
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt = fname; _ }; _ }; _ } ]) ->
        (match find_sub fname marker with
         | Some i ->
           let meth = String.sub fname 0 i in
           let rest = String.sub fname (i + String.length marker) (String.length fname - i - String.length marker) in
           let rec split s =
             match find_sub s "__" with
             | Some j -> String.sub s 0 j :: split (String.sub s (j + 2) (String.length s - j - 2))
             | None -> [ s ]
           in
           Hashtbl.add table meth (split rest, fname)
         | None -> ())
      | _ -> ())
    structure;
  table

(* Before any probe: drops the library items (anything not written in the
   user's own file) that nothing the program can reach depends on, so the
   probe rounds only ever type what's really used. Same walk as [reachable],
   plus what a trait call reaches without naming it: a used trait method
   keeps its trait, and that trait's impls / overloads -- but only those for
   types that are themselves reached (a `showable of matrix` is useless
   while nothing builds a matrix). *)
let prune_for_probe (user_file : string) (structure : Parsetree.structure) : Parsetree.structure =
  let methods = collect_trait_methods structure in
  let trait_of_method = Hashtbl.create 64 in
  List.iter (fun (m, t, _, _) -> Hashtbl.replace trait_of_method m t) methods;
  let traits = Hashtbl.create 16 in
  List.iter (fun (_, t, _, _) -> Hashtbl.replace traits t ()) methods;
  let declared_types = Hashtbl.create 16 in
  List.iter
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_type (_, decls) -> List.iter (fun (d : Parsetree.type_declaration) -> Hashtbl.replace declared_types d.ptype_name.txt ()) decls
      | _ -> ())
    structure;
  let overloads = collect_overloads structure in
  (* `conversionnable_matrix__array` -> [ "conversionnable_matrix"; "array" ]:
     at each `__`, not stopping at a single `_`. *)
  let rec split_on s =
    let n = String.length s in
    let rec find i = if i + 1 >= n then None else if s.[i] = '_' && s.[i + 1] = '_' then Some i else find (i + 1) in
    match find 0 with
    | Some j -> String.sub s 0 j :: split_on (String.sub s (j + 2) (n - j - 2))
    | None -> [ s ]
  in
  (* What would make an item worth keeping without anything naming it:
     `Some (trigger, types)` for an impl module (its trait) or an overload
     (its method). *)
  let trigger_of (item : Parsetree.structure_item) =
    match item.pstr_desc with
    | Pstr_module { pmb_name = { txt = Some name; _ }; _ } ->
      (match split_on (String.uncapitalize_ascii name) with
       | t :: tys when tys <> [] && Hashtbl.mem traits t ->
         (* an impl on a nested type: every type of its pattern *)
         (match List.find_opt (fun (i : SCaml.Impl_pattern.impl) -> i.module_name = name) (SCaml.Impl_pattern.impls_of t) with
          | Some impl ->
            let rec types = function
              | SCaml.Impl_pattern.Var _ -> []
              | Con (c, args) -> c :: List.concat_map types args
            in
            Some (t, types impl.pattern)
          | None -> Some (t, tys))
       | _ -> None)
    | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; _ } ]) ->
      Hashtbl.fold (fun m (tys, fname) acc -> if fname = txt then Some (m, tys) else acc) overloads None
    | _ -> None
  in
  let prunable (item : Parsetree.structure_item) =
    item.pstr_loc.loc_start.pos_fname <> user_file
    && (match item.pstr_desc with Pstr_value _ | Pstr_module _ | Pstr_modtype _ | Pstr_type _ -> true | _ -> false)
  in
  let items = Array.of_list structure in
  let refs = Array.map referenced items in
  let triggers = Array.map trigger_of items in
  let provider_of_name = Hashtbl.create 256 in
  Array.iteri (fun i item -> List.iter (fun n -> Hashtbl.add provider_of_name n i) (provided_names item)) items;
  let kept = Array.make (Array.length items) false in
  let used = Hashtbl.create 32 (* trait and method names *) in
  let reached_types = Hashtbl.create 16 in
  let rec visit i =
    if not kept.(i) then begin
      kept.(i) <- true;
      (match items.(i).pstr_desc with
       | Pstr_type (_, decls) -> List.iter (fun (d : Parsetree.type_declaration) -> Hashtbl.replace reached_types d.ptype_name.txt ()) decls
       | _ -> ());
      List.iter
        (fun (n, _) ->
          List.iter visit (Hashtbl.find_all provider_of_name n);
          (match Hashtbl.find_opt trait_of_method n with
           | Some t -> Hashtbl.replace used n (); Hashtbl.replace used t (); List.iter visit (Hashtbl.find_all provider_of_name t)
           | None -> ());
          if Hashtbl.mem traits n then Hashtbl.replace used n ())
        refs.(i)
    end
  in
  Array.iteri (fun i item -> if not (prunable item) then visit i) items;
  (* Using a trait or a type can unlock impls, whose bodies can use more:
     until nothing changes. *)
  let rec fixpoint () =
    let changed = ref false in
    Array.iteri
      (fun i trig ->
        match trig with
        | Some (t, tys) when (not kept.(i)) && Hashtbl.mem used t
                             && List.for_all (fun ty -> not (Hashtbl.mem declared_types ty) || Hashtbl.mem reached_types ty) tys ->
          visit i; changed := true
        | _ -> ())
      triggers;
    if !changed then fixpoint ()
  in
  fixpoint ();
  List.filteri (fun i _ -> kept.(i)) structure

(* True exactly when [path] is a direct projection of the impl functor
   parameter every `impl` in lib/parser.mly names "X" (e.g. `X.__elem0__`,
   `X.a`) -- i.e. [ty] is still abstract because we're looking at it from
   *inside* the generic impl itself, before it's been instantiated with any
   particular concrete type. Checked by name, not by tracking the exact
   [Ident.t] through the walk: impls never nest, so at most one such "X" is
   ever in scope at a time. *)
let path_is_x_field (path : Path.t) : string option =
  match path with
  | Pdot (Pident id, field) when Ident.name id = "X" -> Some field
  | _ -> None

(* A type seen from inside a generic impl, built from known types and that
   impl's own functor parameter fields (`X.a`), at any depth: `X.a array
   array` is `XCon ("array", [ XCon ("array", [ XField "a" ]) ])`. *)
type xtree =
  | XField of string
  | XCon of string * xtree list

(* [find_impl] over a type built on the enclosing impl's functor parameter
   (`X.a array array`): a field, still unknown, only matches a parameter.
   An impl that needs it to be some type (`int array` for an `X.a array`)
   can't be told to apply here, inside generic code: the usual impl is
   used. *)
let find_impl_x trait (tree : xtree) : (string * xtree list) option =
  let rec bind (p : SCaml.Impl_pattern.t) t acc =
    match p, t with
    | Var v, _ -> Some ((v, t) :: acc)
    | Con (c, ps), XCon (n, args) when c = n && List.compare_lengths ps args = 0 ->
      List.fold_left2 (fun acc p t -> Option.bind acc (bind p t)) (Some acc) ps args
    | Con _, _ -> None
  in
  find_impl_with bind trait tree

let rec xtree_has_field = function
  | XField _ -> true
  | XCon (_, args) -> List.exists xtree_has_field args

(* [ty] as an [xtree], if every leaf is a known type or an `X.<field>`; a
   `tupleN` stands for a tuple, as in [type_tree]. *)
let rec classify_x (env : Env.t) (ty : Types.type_expr) : xtree option =
  let ty = Ctype.expand_head env ty in
  let all f l =
    let r = List.map f l in
    if List.for_all Option.is_some r then Some (List.map Option.get r) else None
  in
  match Types.get_desc ty with
  | Tconstr (path, [], _) when path_is_x_field path <> None -> Option.map (fun f -> XField f) (path_is_x_field path)
  | Tconstr (path, args, _) ->
    (match known_type_name path with
     | Some n -> Option.map (fun args -> XCon (n, args)) (all (classify_x env) args)
     | None -> None)
  | Ttuple components ->
    Option.map
      (fun args -> XCon (Printf.sprintf "tuple%d" (List.length args), args))
      (all (fun (_, t) -> classify_x env t) components)
  | _ -> None

(* Every `module type` again (see [collect_trait_methods]): the abstract
   type names it declares, in order -- the same list [item]'s TRAIT rule in
   lib/parser.mly computed and used to decide the "self" vs. "other" types
   for a functor-shaped impl. Re-derived here from the Parsetree rather than
   shared with the parser, consistent with how [collect_trait_methods]
   already works. *)
let collect_trait_abstract_types (structure : Parsetree.structure) : (string, string list) Hashtbl.t =
  let table = Hashtbl.create 16 in
  List.iter
    (fun (item : Parsetree.structure_item) ->
      match item.pstr_desc with
      | Pstr_modtype { pmtd_name; pmtd_type = Some { pmty_desc = Pmty_signature sigs; _ }; _ } ->
        let names =
          List.concat_map
            (fun (si : Parsetree.signature_item) ->
              match si.psig_desc with
              | Psig_type (_, decls) -> List.map (fun d -> d.Parsetree.ptype_name.txt) decls
              | _ -> [])
            sigs
        in
        Hashtbl.replace table pmtd_name.txt names
      | _ -> ())
    structure;
  table

(* A functor argument field's value: either a concrete, globally-named type
   (`Concrete "int"`), or, from *inside* another impl's own generic body, a
   projection of that impl's own functor parameter (`ViaX "__elem0__"`,
   i.e. `X.__elem0__`) -- deferred until that enclosing impl itself gets
   instantiated. *)
type concrete_ref =
  | Concrete of string
  | ViaX of string
  (* A full type with its arguments (`int list` for a fold's `acc`), not
     just a head name, which on its own (`type acc = list`) isn't a type. *)
  | ConcreteTree of type_tree
  (* A type built on that functor parameter, at any depth (`X.a array`, for
     `iter` on an `X.a array array`). *)
  | ViaXTree of xtree

(* [tree] as a type expression: `Ty ("list", [Ty ("int", [])])` is `int
   list`, a `tupleN` is a tuple. *)
let rec core_of_tree loc (Ty (n, args)) : Parsetree.core_type =
  let args = List.map (core_of_tree loc) args in
  if String.length n > 5 && String.sub n 0 5 = "tuple" then Ast_helper.Typ.tuple ~loc (List.map (fun t -> (None, t)) args)
  else Ast_helper.Typ.constr ~loc (Location.mkloc (SCaml.Type_name.lid n) loc) args

(* The argument a functor-shaped impl is applied to at a call site: either a
   fresh anonymous struct binding each "other" type field (the common
   case), or, when that impl's body needs *values* (not just a type) from
   its "other" parameter (e.g. `showable of array` recursing `print` on
   elements), a direct reference to an already-existing impl module that
   itself satisfies the needed trait (e.g. `Showable__int`) -- see
   [upgrade_dict_impls]. That dictionary is itself a functor application
   when the element type is a container too (e.g. `Showable__array
   (Showable__int)` for an `int array array`). *)
type dict_module = Dict of string * dict_module list

type functor_arg =
  | FieldStruct of (string * concrete_ref) list
  | DictModule of dict_module

(* Where a trait method call resolves to: a plain module (no "other"
   abstract type, e.g. `Arithm__int`), a functor that needs applying inline
   at this call site (e.g. `Iterable__array` applied to `struct type a =
   int end`, for `iter` used on an `int array`), or -- only ever recorded
   from *inside* a generic impl's own body -- a direct projection of that
   impl's own functor parameter (`println` -> `X.println`, deferred the
   same way). *)
type dispatch_target =
  | Plain of string
  | Functored of string * functor_arg
  (* The dictionary module, `X` (or `X__k`, see [upgrade_dict_impls]), and
     the method. *)
  | ViaDictParam of string * string
  | Overload of string
  (* A whole dictionary built at the call site, e.g. `Showable__tuple2
     (Showable__string) (Showable__int)` for `println ("a", 1)`. *)
  | DictCall of dict_module
  (* A call that can never run, on a value of the empty type (see
     [absurd_free_type_vars]), to that many arguments: `fun _ _ -> assert
     false` (taking them, rather than a bare `assert false`, so OCaml doesn't
     warn that they're unused). *)
  | Absurd of int

(* Identifies a source span well enough to match the same node between the
   probe's Typedtree and the original Parsetree (both come from the exact
   same source text, so positions line up exactly). *)
let loc_key (loc : Location.t) =
  (loc.loc_start.pos_fname, loc.loc_start.pos_cnum, loc.loc_end.pos_cnum)

(* What a trait method call's self argument turned out to be. *)
type self_kind = Unknown | Function | Other

(* Every trait method call [harvest_dispatch] has seen, keyed by the
   method's own location: its name, trait, and its self argument's type
   (printed) and kind. Left unresolved -- no impl for that type -- such a
   call reaches OCaml's typechecker as a bare name and fails as "Unbound
   value"; [report_type_error] reads this to say which impl is missing. *)
let trait_calls : (string * int * int, string * string * string * self_kind) Hashtbl.t = Hashtbl.create 16

let record_trait_call loc name trait_name env ty =
  (* Expanded, so an impl's own alias (`Showable__string.a`) reads as the
     type it stands for (`string`). *)
  let ty = Ctype.expand_head env ty in
  let kind =
    match Types.get_desc ty with
    | Tvar _ -> Unknown
    | Tarrow _ -> Function
    | _ -> Other
  in
  Hashtbl.replace trait_calls (loc_key loc) (name, trait_name, Format.asprintf "%a" Printtyp.type_expr ty, kind)

(* Per typed item of the last round's probe: the requirement count it was
   walked with, and what walking it added (dispatches, requirements,
   misses). See [harvest_dispatch]. *)
let harvest_cache :
    (Typedtree.structure_item * (int * int * bool) * ((string * int * int) * dispatch_target) list * (string * (string * string)) list
     * (Location.t * string * string * string list) list * string list) list ref =
  ref []

(* The impl modules whose body still holds a trait call left to dispatch
   (not one to the impl's own method): what they need from their functor
   parameter may still grow -- `print x` on an element deep in callbacks is
   only found once those are dispatched. A call that would pass one of them
   a bare type ([FieldStruct]) waits until it's done, rather than miss the
   dictionary found later. Unless nothing else moves: then
   [defer_to_pending] is off for a round. Found by [harvest_dispatch] as it
   walks them, before the code using them. [pending_version] changes with
   [defer_to_pending], for [harvest_cache]. *)
let pending_impls : (string, unit) Hashtbl.t = Hashtbl.create 16
let defer_to_pending = ref true
let pending_version = ref 0
let deferred_count = ref 0

(* The items (by location) the last [harvest_dispatch] really walked: only
   those can have something new to rewrite. *)
let harvest_walked : (string * int * int) list ref = ref []

(* Walks the probed Typedtree looking for `Texp_apply` whose function is
   directly a trait method name (e.g. `+ a b`, `print x`, or the innermost
   application in a curried multi-arg call). For each one found, reads the
   inferred type of its first real argument and records, keyed by the
   location of that method-name identifier, which impl module it resolves
   to -- e.g. `+` applied to two [int]s records "Arithm__int" at that spot. *)
let harvest_dispatch ?(cache = false)
    (methods : (string * string * string list * (string * int) option list) list)
    (abstract_types : (string, string list) Hashtbl.t)
    (ctor_methods : (string, ctor_method) Hashtbl.t)
    (overloads : (string, string list * string) Hashtbl.t)
    (prior_dict_requirements : (string, (string * string) list) Hashtbl.t)
    (typed : Typedtree.structure) =
  let trait_of_name = Hashtbl.create 16 in
  let arity_of_name = Hashtbl.create 16 in
  let domains_of_name = Hashtbl.create 16 in
  let names_of_name = Hashtbl.create 16 in
  List.iter
    (fun (name, trait, names, domains) ->
      Hashtbl.replace trait_of_name name trait;
      Hashtbl.replace arity_of_name name (List.length names - 1);
      Hashtbl.replace domains_of_name name domains;
      Hashtbl.replace names_of_name name names)
    methods;
  let table : (string * int * int, dispatch_target) Hashtbl.t = Hashtbl.create 16 in
  (* What the item being walked adds, for [harvest_cache]. *)
  let entry_log = ref [] and req_log = ref [] and pending_log = ref [] in
  let record_entry key target =
    Hashtbl.replace table key target;
    entry_log := (key, target) :: !entry_log
  in
  (* Per generic impl module (keyed by its name, e.g. "Showable__array"):
     the one padding field of its own functor parameter that some call
     inside its body needs a *value* from (not just a type), and which
     trait that call needs it to satisfy -- e.g. `print e` on an array
     element records ("__elem0__", "showable"). Filled in here, consumed by
     [upgrade_dict_impls] (which upgrades that impl's functor signature
     accordingly) and, right below, by this same function's own handling of
     *external* call sites dispatching to that impl (which must then pass
     an existing impl module as the functor argument, not a bare struct). *)
  let dict_requirements : (string, (string * string) list) Hashtbl.t = Hashtbl.copy prior_dict_requirements in
  (* Overloaded calls whose argument types are all known but match no impl:
     location, trait, those types. *)
  let overload_misses : (Location.t * string * string * string list) list ref = ref [] in
  (* First one wins: on a later probe round, an impl's body was already
     rewritten and upgraded (its `X.__elem0__` renamed to `X.a`), and must
     not re-record its requirement under that new name. *)
  let add_dict_requirement mod_name req =
    let before = Hashtbl.find_opt dict_requirements mod_name in
    (match before with
    | None -> Hashtbl.replace dict_requirements mod_name [ req ]
    (* An impl with several parameters (`showable of (k, v) Hashtbl.t`)
       can need one per parameter. *)
    | Some reqs when is_multi_impl mod_name && not (List.mem_assoc (fst req) reqs) ->
      Hashtbl.replace dict_requirements mod_name (reqs @ [ req ])
    | Some _ -> ());
    if Hashtbl.find_opt dict_requirements mod_name != before then req_log := (mod_name, req) :: !req_log
  in
  (* The traits the dictionaries passed to impl [m] for its [n] type
     arguments must satisfy: each its own, for an impl needing one per
     parameter, or else the one its element needs ([default] if none). *)
  let arg_traits m default n =
    match Hashtbl.find_opt dict_requirements m with
    | Some reqs when multi_complete m reqs -> List.map (fun p -> List.assoc p reqs) (Hashtbl.find impl_functor_params m)
    | Some ((_, t) :: _) -> List.init n (fun _ -> t)
    | _ -> List.init n (fun _ -> default)
  in
  let other_names_of trait_name =
    match Hashtbl.find_opt abstract_types trait_name with
    | Some all -> (match List.rev all with _ :: rest -> List.rev rest | [] -> [])
    | None -> []
  in
  let self_name_of trait_name =
    match Hashtbl.find_opt abstract_types trait_name with
    | Some all -> (match List.rev all with last :: _ -> Some last | [] -> None)
    | None -> None
  in
  (* Mirrors lib/parser.mly's IMPL rule exactly: not every "other" abstract
     type is tied to the concrete type's own constructor arguments (e.g.
     `foldable { type acc a b }`'s `acc` has nothing to do with `array`'s
     element type -- only `a`, the *last* one before self, does); only the
     last [arity] of [other_names] are "tied". [ty] may still need *more*
     type parameters than it has tied names for -- pad with fresh,
     trait-invisible names for the remainder, named exactly as
     lib/parser.mly's IMPL rule names them ("__elem0__", ...), so the two
     agree on what a functor instantiation here needs to supply. *)
  let split_other_names other_names arity =
    let tied_count = min arity (List.length other_names) in
    let other_count = List.length other_names in
    let free_names = List.filteri (fun i _ -> i < other_count - tied_count) other_names in
    let tied_names = List.filteri (fun i _ -> i >= other_count - tied_count) other_names in
    (free_names, tied_names)
  in
  let all_param_names other_names arity =
    let _free_names, tied_names = split_other_names other_names arity in
    let pad_count = max 0 (arity - List.length tied_names) in
    let pad_names = List.init pad_count (fun i -> Printf.sprintf "__elem%d__" (List.length tied_names + i)) in
    tied_names @ pad_names
  in
  (* The impl module currently being walked, if any (e.g. "Showable__array"
     while inside its `struct ... end`). A dispatch target whose module is
     *this* one must be left as a plain local reference instead -- from
     inside its own still-being-defined struct, referring to the module by
     name is "Unbound module". Any *other* impl's module (already fully
     defined earlier) is fine to reference qualified, which is exactly what
     lets e.g. `showable of array`'s `print` call `Showable__string.print`
     or the generic element's impl for a call like `print "[ "` / `print e`
     inside its own body. *)
  let current_impl_module = ref None in
  let self_hit = ref false in
  let pending_check = ref None in
  let target_is_self mod_name =
    let self = !current_impl_module = Some mod_name in
    if self then self_hit := true;
    self
  in
  (* See [pending_impls]. *)
  let defers mod_name =
    !defer_to_pending && Hashtbl.mem pending_impls mod_name && not (target_is_self mod_name)
    && (incr deferred_count; true)
  in
  (* The functor parameter a value of type `X.<field>` takes its
     dictionary from, in the impl being walked. *)
  let dict_param field =
    match !current_impl_module with
    | Some m when is_multi_impl m -> multi_dict_param field
    | _ -> "X"
  in
  (* A call on a type with no impl (e.g. `println p` for a user type
     without `impl showable`) is left alone, so it fails as an unresolved
     trait call, reported at the call (see [trait_calls]), rather than as
     an "Unbound module" with no location. *)
  let impl_exists env mod_name =
    match Env.find_module_by_name (Longident.Lident mod_name) env with
    | _ -> true
    | exception Not_found -> false
  in
  (* `iter f arr` is curried: `App(App(iter, f), arr)`, i.e. *two* nested
     Texp_apply nodes each carrying exactly one argument -- so a call site
     can only be inspected as a whole once all of a method's arguments have
     been collected by walking back through this chain, not just from the
     one Texp_apply whose own function happens to be the bare identifier
     (which, for an arity > 1 method, only ever sees the first argument). *)
  let rec flatten_apply (e : Typedtree.expression) : Typedtree.expression * Typedtree.expression list =
    match e.exp_desc with
    | Texp_apply (f, args) ->
      let real_args = List.filter_map (function (_, Typedtree.Arg a) -> Some a | _ -> None) args in
      let head, prior = flatten_apply f in
      (head, prior @ real_args)
    | _ -> (e, [])
  in
  (* The bound variable and body of the [level]-th parameter of a curried
     `fun x y .. -> body`-shaped lambda (each parameter a plain, single,
     irrefutable one; [level] 0 is the first). Matches [param_domains]'s own
     level for a given trait method parameter, so this finds e.g. `iteri`'s
     callback's *second* parameter (the element, level 1), not its first
     (the index, which [fn_param_domain] already skipped over). *)
  let rec lambda_param_and_body (level : int) (e : Typedtree.expression) : (Ident.t * Typedtree.expression) option =
    match e.exp_desc with
    | Texp_function ([ { fp_param; _ } ], Tfunction_body body) ->
      if level = 0 then Some (fp_param, body) else lambda_param_and_body (level - 1) body
    | _ -> None
  in
  (* For a callback passed to a container-iterating method (e.g. `iter`'s
     first argument): walks its body for trait-method calls made *directly*
     on the callback's own bound parameter (e.g. `print e`) -- the element
     value itself, so, like [`DirectX] above, dispatching it needs a value
     from the enclosing generic impl's functor parameter, not just a type.
     Can't be found through probing at all here (the probe's necessarily
     generic `iter` stub doesn't relate its callback's domain to its
     container's element type the way the real [Array.iter] does), so this
     matches the callback's bound [Ident.t] directly instead of inspecting
     any (nonexistent, at this point) concrete type. *)
  let scan_callback_body (callback_ident : Ident.t) (field : string) (body : Typedtree.expression) =
    let expr2 (it2 : Tast_iterator.iterator) (e2 : Typedtree.expression) =
      (match e2.exp_desc with
       | Texp_apply _ ->
         let head2, all_args2 = flatten_apply e2 in
         (match head2.exp_desc with
          | Texp_ident (_, lid2, _) ->
            let name2 = Longident.last lid2.txt in
            (match Hashtbl.find_opt trait_of_name name2, Hashtbl.find_opt arity_of_name name2 with
             | Some trait_name2, Some arity2
               when arity2 > 0 && List.length all_args2 >= arity2 && not (Hashtbl.mem overloads name2) ->
               let dispatch_args2 = List.filteri (fun i _ -> i < arity2) all_args2 in
               let is_callback_var (a : Typedtree.expression) =
                 match a.exp_desc with
                 | Texp_ident (Pident id, _, _) -> Ident.same id callback_ident
                 | _ -> false
               in
               if List.exists is_callback_var dispatch_args2 then begin
                 record_entry (loc_key head2.exp_loc) (ViaDictParam (dict_param field, name2));
                 match !current_impl_module with
                 | Some mod_name2 -> add_dict_requirement mod_name2 (field, trait_name2)
                 | None -> ()
               end
             | _ -> ())
          | _ -> ())
       | _ -> ());
      Tast_iterator.default_iterator.expr it2 e2
    in
    let it2 = { Tast_iterator.default_iterator with expr = expr2 } in
    it2.expr it2 body
  in
  (* Position of [name]'s self-type parameter among its first [arity]
     parameters (e.g. `b`, the third, for `fold_left of (acc -> a -> acc)
     -> acc -> b -> acc`), via [names_of_name]. *)
  let self_pos_of name trait_name arity =
    match Hashtbl.find_opt names_of_name name, self_name_of trait_name with
    | Some names, Some self_name ->
      let param_names = List.filteri (fun i _ -> i < arity) names in
      let rec idx i = function [] -> None | n :: rest -> if n = self_name then Some i else idx (i + 1) rest in
      idx 0 param_names
    | _ -> None
  in
  (* Where a call to trait method [name] resolves to, once its self type is
     known to be concretely [type_name] applied to [arg_trees]. [arg_tys]
     are the types of the method's first [arity] parameters at this use
     site (to resolve "free" other names, e.g. `foldable`'s `acc`). Returns
     the impl module name, those free names' resolutions, and the target. *)
  let concrete_target name trait_name arity (arg_tys : (Env.t * Types.type_expr) list) type_name arg_trees =
    let other_names = other_names_of trait_name in
    let mod_name, arg_trees = impl_for trait_name (Ty (type_name, arg_trees)) in
    let arg_type_names = List.map ty_name arg_trees in
    let params =
      match Hashtbl.find_opt impl_functor_params mod_name with
      | Some ps -> ps
      | None -> all_param_names other_names (List.length arg_type_names)
    in
    (* A "free" other name (e.g. `foldable`'s `acc`) isn't part
       of the self type's own structure: impls are polymorphic in
       it (see lib/parser.mly's IMPL rule), so it's never passed
       to their functor. It's still resolved here when this call
       shows it (e.g. the `0` in `fold_left (+) 0 arr`), only to
       dispatch a bare callback argument typed by it, below. *)
    let free_names, _tied_names = split_other_names other_names (List.length arg_type_names) in
    let free_values =
      List.filter_map
        (fun fn ->
          match Hashtbl.find_opt names_of_name name with
          | Some all_names ->
            let param_type_names = List.filteri (fun i _ -> i < arity) all_names in
            let rec idx i = function [] -> None | n :: rest -> if n = fn then Some i else idx (i + 1) rest in
            (match idx 0 param_type_names with
             | Some i ->
               (match List.nth_opt arg_tys i with
                | Some (arg_env, arg_ty) ->
                  (match classify_texpr arg_env arg_ty with
                   | Some (Ty (tn, [])) -> Some (fn, Concrete tn)
                   | Some tree -> Some (fn, ConcreteTree tree)
                   | None -> None)
                | None -> None)
             | None -> None)
          | None -> None)
        free_names
    in
    let target =
      if params = [] then Some (Plain mod_name)
      else if List.length params = List.length arg_type_names then
        (* The full element type (`int list`, `int * int`), not just its
           head name, which alone (`type a = tuple2`) isn't a type. *)
        let field_of (Ty (n, args) as tree) =
          if args = [] && not (Hashtbl.mem tuple_impls (impl_module_name "showable" n)) then Concrete n
          else ConcreteTree tree
        in
        let full_fields = List.combine params (List.map field_of arg_trees) in
        (* The whole functor argument is itself a dictionary when
           it has exactly one field overall (whether that field
           is "tied" to the self type, like `sumable`'s `a`, or
           pure padding, like `showable of array`'s) and that
           field needs one: pass the *existing* impl of
           [needed_trait] for its concrete type directly (e.g.
           `Arithm__int`), instead of an anonymous `struct type
           ... end` it couldn't actually satisfy. *)
        (* A nested container element (e.g. the `int array` of
           an `int array array`) is itself a functor-shaped
           impl: apply it, recursively, to the dictionary its
           own element needs, rather than passing it bare. *)
        let rec dict_of needed_trait tree =
          let dict_mod, args = impl_for needed_trait tree in
          Dict (dict_mod, List.map2 dict_of (arg_traits dict_mod needed_trait (List.length args)) args)
        in
        (match full_fields, Hashtbl.find_opt dict_requirements mod_name, arg_trees with
         (* One dictionary per parameter: `Showable__Hashtbl't
            (Showable__string) (Showable__int)`. *)
         | _, Some reqs, _ when multi_complete mod_name reqs ->
           Some (DictCall (Dict (mod_name, List.map2 dict_of (arg_traits mod_name trait_name (List.length arg_trees)) arg_trees)))
         | [ (only_field, (Concrete _ | ConcreteTree _)) ], Some [ (dict_field, needed_trait) ], [ elem_tree ]
           when dict_field = only_field ->
           Some (Functored (mod_name, DictModule (dict_of needed_trait elem_tree)))
         | [ (only_field, Concrete concrete_type) ], Some [ (dict_field, needed_trait) ], _ when dict_field = only_field ->
           Some (Functored (mod_name, DictModule (Dict (impl_module_name needed_trait concrete_type, []))))
         | _ when defers mod_name -> None
         | _ -> Some (Functored (mod_name, FieldStruct full_fields)))
      else Some (Plain mod_name)
    in
    (mod_name, free_values, target)
  in
  (* A call to a constructor-trait method (e.g. `map`, from `mappable`'s
     `('a -> 'b) -> 'a t -> 'b t`): finds, among the actual parameter types
     at this use site ([arg_tys]), the one matching a `... t` position in
     the declared signature, and dispatches to that constructor's impl
     (e.g. `int array` -> `Mappable__array`). *)
  let resolve_ctor (cm : ctor_method) (arg_tys : (Env.t * Types.type_expr) list) : dispatch_target option =
    let found = ref None in
    let rec go env (s : Parsetree.core_type) ty =
      let ty = Ctype.expand_head env (strip_tpoly ty) in
      match s.ptyp_desc, Types.get_desc ty with
      | Ptyp_arrow (_, s1, s2), Tarrow (_, t1, t2, _) -> go env s1 t1; go env s2 t2
      | Ptyp_constr ({ txt = Longident.Lident c; _ }, sargs), Tconstr (path, targs, _)
        when c = cm.cm_ctor && List.length sargs = List.length targs && !found = None ->
        (* Only the constructor matters, so classify it with its arguments
           replaced by `int` -- they may well still be unknown here. *)
        (match classify_texpr env (Ctype.newconstr path (List.map (fun _ -> Predef.type_int) targs)) with
         | Some (Ty (n, _)) -> found := Some n
         | None -> ())
      | _ -> ()
    in
    let rec sig_params n (s : Parsetree.core_type) =
      if n = 0 then [] else match s.ptyp_desc with Ptyp_arrow (_, s1, s2) -> s1 :: sig_params (n - 1) s2 | _ -> []
    in
    let sparams = sig_params (List.length arg_tys) cm.cm_sig in
    if List.length sparams = List.length arg_tys then List.iter2 (fun s (env, ty) -> go env s ty) sparams arg_tys;
    match !found with
    | Some ctor_ty ->
      let mod_name = impl_module_name cm.cm_trait ctor_ty in
      if target_is_self mod_name then None else Some (Plain mod_name)
    | None -> None
  in
  (* A call to an overloaded method: the overload whose parameter types
     match the outermost constructors of the actual arguments' types (e.g.
     `int array`, `int` -> `array, int`). None while any is still unknown. *)
  let resolve_overload loc name (arg_tys : (Env.t * Types.type_expr) list) : dispatch_target option =
    match Hashtbl.find_all overloads name with
    | [] -> None
    | ((tys, _) :: _) as candidates ->
      let n = List.length tys in
      if List.length arg_tys < n then None
      else
        (* Each argument's type: named (`int`, `array`), still unknown (a
           type variable), or known but not by a name an overload can list
           (a tuple, a function...), which only a `_` matches. *)
        let heads =
          List.map
            (fun (env, ty) ->
              let ty = strip_tpoly ty in
              match head_type_name env ty, Types.get_desc (Ctype.expand_head env ty) with
              | Some n, _ -> `Named n
              | None, Tvar _ -> `Unknown
              | None, _ -> `Unnamed (Format.asprintf "%a" Printtyp.type_expr ty))
            (List.filteri (fun i _ -> i < n) arg_tys)
        in
        let is_wildcard ty = ty = SCaml.Type_name.wildcard in
        (* An unknown type some overload names at its position: wait. *)
        let must_wait =
          List.exists
            (fun (tys, _) -> List.exists2 (fun ty h -> h = `Unknown && not (is_wildcard ty)) tys heads)
            candidates
        in
        if must_wait then None
        else
          let matches (tys, _) =
            List.for_all2 (fun ty h -> is_wildcard ty || h = `Named ty) tys heads
          in
          (* The most specific one: `Hashtbl.t, range` before `Hashtbl.t, _`. *)
          let wildcards (tys, _) = List.length (List.filter is_wildcard tys) in
          match List.sort (fun a b -> compare (wildcards a) (wildcards b)) (List.filter matches candidates) with
          | (_, fname) :: _ -> Some (Overload fname)
          | [] when List.mem `Unknown heads -> None
          | [] ->
            (* Every type is known and still nothing matches: that's final. *)
            let trait_name = Option.value ~default:"?" (Hashtbl.find_opt trait_of_name name) in
            let shown = List.map (function `Named n -> n | `Unnamed t -> t | `Unknown -> "_") heads in
            overload_misses := (loc, trait_name, name, shown) :: !overload_misses;
            None
  in
  let expr (iter : Tast_iterator.iterator) (e : Typedtree.expression) =
    dispatch_loc := e.exp_loc;
    self_hit := false;
    (* A trait call inside an impl, left without a target this time (and not
       to the impl's own method): the impl is pending. *)
    (match e.exp_desc, !current_impl_module with
     | Texp_apply _, Some m when cache ->
       let head, _ = flatten_apply e in
       (match head.exp_desc with
        | Texp_ident (_, { txt = Longident.Lident name; _ }, _) when Hashtbl.mem trait_of_name name ->
          pending_check := Some (m, loc_key head.exp_loc)
        | _ -> pending_check := None)
     | _ -> pending_check := None);
    (match e.exp_desc with
     | Texp_apply _ ->
       let head, all_args = flatten_apply e in
       (match head.exp_desc with
        | Texp_ident (_, lid, _) ->
          let name = Longident.last lid.txt in
          (match Hashtbl.find_opt trait_of_name name, Hashtbl.find_opt arity_of_name name with
           | Some _, Some _
             when !absurd_free_type_vars
                  && List.exists (fun (a : Typedtree.expression) -> is_empty_type a.exp_env a.exp_type) all_args ->
             record_entry (loc_key head.exp_loc) (Absurd (List.length all_args))
           | Some _, Some _ when Hashtbl.mem overloads name ->
             let arg_tys = List.map (fun (a : Typedtree.expression) -> (a.exp_env, a.exp_type)) all_args in
             (match resolve_overload head.exp_loc name arg_tys with
              | Some t -> record_entry (loc_key head.exp_loc) t
              | None -> ())
           | Some _, Some arity when arity > 0 && List.length all_args >= arity && Hashtbl.mem ctor_methods name ->
             let dispatch_args = List.filteri (fun i _ -> i < arity) all_args in
             let arg_tys = List.map (fun (a : Typedtree.expression) -> (a.exp_env, a.exp_type)) dispatch_args in
             (match resolve_ctor (Hashtbl.find ctor_methods name) arg_tys with
              | Some t -> record_entry (loc_key head.exp_loc) t
              | None -> ())
           | Some trait_name, Some arity when arity > 0 && List.length all_args >= arity ->
             let dispatch_args = List.filteri (fun i _ -> i < arity) all_args in
             let other_names = other_names_of trait_name in
             (* Not "whichever argument happens to classify" (e.g. for
                `fold_left (+) 0 arr`, the accumulator `0` classifies as
                `int` too, and sits *before* the real container `arr` --
                picking the first classifiable argument would wrongly
                dispatch on `int`): the self type is a specific, known
                position in the method's own signature (e.g. `b`, for
                `fold_left of (acc -> a -> acc) -> acc -> b -> acc`), found
                via [names_of_name] the same way [collect_trait_methods]
                built it. Classifies concretely, or, from *inside* another
                generic impl's own body, as still-abstract via that impl's
                own functor parameter (see [path_is_x_field]): either
                directly (`DirectX`, e.g. `print e` on an array element), or
                wrapped in a known container (`ContainerX`, e.g. `iter`'s
                own `b` argument when it's `X.__elem0__ array`). *)
             let classify_at (a : Typedtree.expression) =
               match classify_texpr a.exp_env a.exp_type with
               | Some (Ty (n, args)) -> `Concrete (n, args)
               | None ->
                 (match Types.get_desc (Ctype.expand_head a.exp_env a.exp_type) with
                  | Tconstr (path, [], _) ->
                    (match path_is_x_field path with Some field -> `DirectX field | None -> `None)
                  (* Any known one-parameter container (`array`, `list`, a
                     user's `a btree`...), not just arrays. *)
                  | Tconstr (path, [ arg_ty ], _) when known_type_name path <> None ->
                    let container = Option.get (known_type_name path) in
                    (match Types.get_desc (Ctype.expand_head a.exp_env arg_ty) with
                     | Tconstr (apath, [], _) when path_is_x_field apath <> None ->
                       `ContainerX (container, Option.get (path_is_x_field apath))
                     (* A container of something itself built on the
                        functor parameter (`X.a array array`, `(int * X.a)
                        list`): the element type is passed whole. *)
                     | _ ->
                       (match classify_x a.exp_env arg_ty with
                        | Some elem when xtree_has_field elem -> `ContainerXTree (container, elem)
                        | _ -> `None))
                  (* A tuple mixing known types and the enclosing impl's own
                     element type (`print (i, a)` inside `showable of rle`,
                     an `int * X.a`): each component known or that field. *)
                  | Ttuple components ->
                    let comps =
                      List.map
                        (fun (_, t) ->
                          match classify_texpr a.exp_env t with
                          | Some tree -> Some (`Known tree)
                          | None ->
                            (match Types.get_desc (Ctype.expand_head a.exp_env t) with
                             | Tconstr (p, [], _) -> Option.map (fun f -> `Field f) (path_is_x_field p)
                             | _ -> None))
                        components
                    in
                    if List.for_all Option.is_some comps then `TupleX (List.map Option.get comps) else `None
                  | _ -> `None)
             in
             let self_pos = self_pos_of name trait_name arity in
             (match Option.bind self_pos (List.nth_opt dispatch_args) with
              | Some (a : Typedtree.expression) -> record_trait_call head.exp_loc name trait_name a.exp_env a.exp_type
              | None -> ());
             (* The self type only in the result (`fn init of int -> a`):
                dispatched on the type the call's use gives it. *)
             let returns_self =
               self_pos = None
               && List.length all_args = arity
               && (match Hashtbl.find_opt names_of_name name, self_name_of trait_name with
                   | Some names, Some self_name -> List.nth_opt names arity = Some self_name
                   | _ -> false)
             in
             if returns_self then record_trait_call head.exp_loc name trait_name e.exp_env e.exp_type;
             let self_result =
               match self_pos with
               | Some i -> (match List.nth_opt dispatch_args i with Some a -> classify_at a | None -> `None)
               | None when returns_self -> classify_at e
               | None ->
                 (* No clean self-position found in the signature (shouldn't
                    normally happen) -- fall back to scanning every
                    argument, as before. *)
                 let rec scan = function [] -> `None | a :: rest -> (match classify_at a with `None -> scan rest | r -> r) in
                 scan dispatch_args
             in
             (match self_result with
              | `Concrete (type_name, arg_trees) when Hashtbl.mem tuple_impls (impl_module_name trait_name type_name) ->
                (* A generated tuple impl: the functor applied, right here, to
                   each component's own dictionary (recursively, so a tuple
                   of arrays or of tuples works too). *)
                let rec dict_for trait tree =
                  let m, args = impl_for trait tree in
                  Dict (m, List.map2 dict_for (arg_traits m trait (List.length args)) args)
                in
                record_entry (loc_key head.exp_loc) (DictCall (dict_for trait_name (Ty (type_name, arg_trees))))
              | `Concrete (type_name, arg_trees) ->
                let arg_tys = List.map (fun (a : Typedtree.expression) -> (a.exp_env, a.exp_type)) dispatch_args in
                let mod_name, free_values, target = concrete_target name trait_name arity arg_tys type_name arg_trees in
                (match target with
                 | Some t when not (target_is_self mod_name) && impl_exists head.exp_env mod_name ->
                   record_entry (loc_key head.exp_loc) t
                 | _ -> ());
                (* A callback argument passed bare (e.g. `println` in `iter
                   println arr`) never appears as the head of its own
                   application, so it can't be resolved the way [target]
                   just was. But if this call's signature says that
                   argument's type is `<abstract> -> ...`, and we just
                   resolved that same abstract placeholder to a concrete
                   type (either as the self type, or as one of the "other"
                   functor types above), and the actual argument is itself a
                   bare identifier naming another trait method, dispatch it
                   directly from that concrete type. *)
                let arg_type_names = List.map ty_name arg_trees in
                let _, _tied_names = split_other_names (other_names_of trait_name) (List.length arg_type_names) in
                let self_name = self_name_of trait_name in
                let concrete_of_abstract =
                  (match self_name with Some s -> [ (s, type_name) ] | None -> [])
                  @ List.filter_map (function n, Concrete t -> Some (n, t) | n, ConcreteTree t -> Some (n, ty_name t) | _, (ViaX _ | ViaXTree _) -> None) free_values
                  @ (try List.combine _tied_names arg_type_names with Invalid_argument _ -> [])
                in
                (match Hashtbl.find_opt domains_of_name name with
                 | Some domains ->
                   List.iteri
                     (fun i domain ->
                       match domain, List.nth_opt dispatch_args i with
                       | Some (abstract_name, _level), Some (arg : Typedtree.expression) ->
                         (match List.assoc_opt abstract_name concrete_of_abstract, arg.exp_desc with
                          | Some concrete_type, Texp_ident (_, arg_lid, _) ->
                            let arg_name = Longident.last arg_lid.txt in
                            (* An overloaded callback (e.g. `(+)`) is resolved
                               from its own instantiated type instead, by the
                               bare-identifier case below. *)
                            (match Hashtbl.find_opt trait_of_name arg_name with
                             | Some arg_trait when not (Hashtbl.mem overloads arg_name) ->
                               let arg_mod_name = impl_module_name arg_trait concrete_type in
                               if not (target_is_self arg_mod_name) then
                                 record_entry (loc_key arg.exp_loc) (Plain arg_mod_name)
                             | _ -> ())
                          | _ -> ())
                       | _ -> ())
                     domains
                 | None -> ())
              | `TupleX comps when Hashtbl.mem tuple_impls (impl_module_name trait_name (Printf.sprintf "tuple%d" (List.length comps))) ->
                (* Like a concrete tuple (see the generated tuple impls), but
                   a component of the impl's own element type takes its
                   dictionary from the impl's functor parameter `X` itself,
                   which [upgrade_dict_impls] then turns into that trait. *)
                let rec dict_for trait tree =
                  let m, args = impl_for trait tree in
                  Dict (m, List.map2 dict_for (arg_traits m trait (List.length args)) args)
                in
                let comp_dicts =
                  List.map
                    (function
                      | `Known tree -> dict_for trait_name tree
                      | `Field field ->
                        (match !current_impl_module with
                         | Some mod_name -> add_dict_requirement mod_name (field, trait_name)
                         | None -> ());
                        Dict (dict_param field, []))
                    comps
                in
                record_entry (loc_key head.exp_loc)
                  (DictCall (Dict (impl_module_name trait_name (Printf.sprintf "tuple%d" (List.length comps)), comp_dicts)))
              | `TupleX _ -> ()
              | `DirectX field ->
                (* `print e` where [e]'s type is *directly* the enclosing
                   impl's own abstract padding field: needs a value, not
                   just a type, so defer straight through that functor
                   parameter ("X") rather than to any named impl module. *)
                record_entry (loc_key head.exp_loc) (ViaDictParam (dict_param field, name));
                (match !current_impl_module with
                 | Some mod_name -> add_dict_requirement mod_name (field, trait_name)
                 | None -> ())
              | `ContainerXTree (base, elem) ->
                (* An impl on the nested type, if one is sure to apply
                   (`e array array` for an `X.a array array`): its own
                   parameters' types, else the usual impl's element. *)
                let mod_name, bound =
                  match find_impl_x trait_name (XCon (base, [ elem ])) with
                  | Some found -> found
                  | None -> (impl_module_name trait_name base, [ elem ])
                in
                let rec dict_for trait = function
                  | XField field ->
                    (match !current_impl_module with
                     | Some m -> add_dict_requirement m (field, trait)
                     | None -> ());
                    Dict (dict_param field, [])
                  | XCon _ as t ->
                    let m, args = match find_impl_x trait t with Some found -> found | None -> (match t with XCon (n, args) -> (impl_module_name trait n, args) | XField _ -> assert false) in
                    Dict (m, List.map2 dict_for (arg_traits m trait (List.length args)) args)
                in
                if not (target_is_self mod_name) then
                  (match Hashtbl.find_opt dict_requirements mod_name, bound with
                   | Some reqs, _ when multi_complete mod_name reqs ->
                     record_entry (loc_key head.exp_loc)
                       (DictCall (Dict (mod_name, List.map2 dict_for (arg_traits mod_name trait_name (List.length bound)) bound)))
                   | Some ((_, needed_trait) :: _), [ elem ] ->
                     (* That impl needs a value from its element (e.g.
                        `showable of array` printing each one): pass the
                        element's own dictionary, built down to the
                        enclosing impl's functor parameter `X`, which then
                        must satisfy that trait too (`print m.rows` on an
                        `X.a array array` -> `Showable__array
                        (Showable__array (X))`). *)
                     record_entry (loc_key head.exp_loc) (Functored (mod_name, DictModule (dict_for needed_trait elem)))
                   | (Some [] | None), _ ->
                     let params =
                       match Hashtbl.find_opt impl_functor_params mod_name with
                       | Some ps -> ps
                       | None -> all_param_names other_names 1
                     in
                     if List.compare_lengths params bound = 0 && not (defers mod_name) then
                       record_entry (loc_key head.exp_loc)
                         (Functored (mod_name, FieldStruct (List.map2 (fun p t -> (p, ViaXTree t)) params bound)))
                   | _ -> ())
              | `ContainerX (base, field) ->
                let mod_name = impl_module_name trait_name base in
                if not (target_is_self mod_name) then
                  (match Hashtbl.find_opt dict_requirements mod_name, all_param_names other_names 1 with
                   (* That impl needs a value from its element (`println
                      d.states` on an `X.s array`, `showable of array`
                      printing each one): the enclosing impl's own
                      dictionary, as for [`ContainerXTree] below. *)
                   | Some ((_, needed_trait) :: _), _ ->
                     (match !current_impl_module with
                      | Some m -> add_dict_requirement m (field, needed_trait)
                      | None -> ());
                     record_entry (loc_key head.exp_loc)
                       (Functored (mod_name, DictModule (Dict (dict_param field, []))))
                   | _, [ only_field ] when not (defers mod_name) ->
                     record_entry (loc_key head.exp_loc) (Functored (mod_name, FieldStruct [ (only_field, ViaX field) ]))
                   | _ -> ());
                (* The container's element type is also `field` -- scan any
                   callback argument (e.g. `iter`'s first argument) for
                   trait-method calls made directly on its own bound
                   element parameter (e.g. `print e`), which probing can't
                   resolve on its own (see [scan_callback_body]). A callback
                   passed *bare*, not as a lambda (e.g. `Array.fold_left
                   (+) acc arr` inside another generic impl, where `(+)`
                   itself names a trait method), is the same situation as
                   [`DirectX] above, just one level removed: it too needs a
                   value from the enclosing impl's own functor parameter. *)
                (match Hashtbl.find_opt domains_of_name name with
                 | Some domains ->
                   List.iteri
                     (fun i domain ->
                       match domain, List.nth_opt dispatch_args i with
                       | Some (_, level), Some (callback_arg : Typedtree.expression) ->
                         (match lambda_param_and_body level callback_arg with
                          | Some (callback_ident, body) -> scan_callback_body callback_ident field body
                          | None ->
                            (match callback_arg.exp_desc with
                             | Texp_ident (_, lid2, _) ->
                               let name2 = Longident.last lid2.txt in
                               (match Hashtbl.find_opt trait_of_name name2 with
                                | Some trait_name2 when not (Hashtbl.mem overloads name2) ->
                                  record_entry (loc_key callback_arg.exp_loc) (ViaDictParam (dict_param field, name2));
                                  (match !current_impl_module with
                                   | Some mod_name2 -> add_dict_requirement mod_name2 (field, trait_name2)
                                   | None -> ())
                                | _ -> ())
                             | _ -> ()))
                       | _ -> ())
                     domains
                 | None -> ())
              | `None -> ())
           | _ -> ())
        | _ -> ())
     (* A trait method used as a bare value, not applied and not a callback
        of another trait method (e.g. `println` in `x |> println`): no
        argument to classify, but its own instantiated type at this use site
        (e.g. `int -> unit`) carries the self type just the same. Visited
        after its enclosing application, so anything already resolved as a
        head or callback above is left alone. *)
     | Texp_ident (_, lid, _) when not (Hashtbl.mem table (loc_key e.exp_loc)) ->
       let name = Longident.last lid.txt in
       (match Hashtbl.find_opt trait_of_name name, Hashtbl.find_opt arity_of_name name with
        | Some trait_name, Some arity when arity > 0 ->
          let rec param_tys n ty =
            if n = 0 then []
            else
              match Types.get_desc (Ctype.expand_head e.exp_env ty) with
              | Tarrow (_, t1, t2, _) -> (e.exp_env, strip_tpoly t1) :: param_tys (n - 1) t2
              | _ -> []
          in
          let arg_tys = param_tys arity e.exp_type in
          (match Hashtbl.find_opt ctor_methods name with
           | _ when Hashtbl.mem overloads name ->
             (match resolve_overload e.exp_loc name arg_tys with
              | Some t -> record_entry (loc_key e.exp_loc) t
              | None -> ())
           | Some cm ->
             (match resolve_ctor cm arg_tys with
              | Some t -> record_entry (loc_key e.exp_loc) t
              | None -> ())
           | None ->
             (match self_pos_of name trait_name arity with
              | Some i when List.length arg_tys = arity ->
                let env, self_ty = List.nth arg_tys i in
                record_trait_call e.exp_loc name trait_name env self_ty;
                (match classify_texpr env self_ty with
                 | Some (Ty (type_name, arg_trees)) ->
                   (match concrete_target name trait_name arity arg_tys type_name arg_trees with
                    | mod_name, _, Some t when not (target_is_self mod_name) && impl_exists e.exp_env mod_name ->
                      record_entry (loc_key e.exp_loc) t
                    | _ -> ())
                 | None ->
                   (* Inside a generic impl, on its own element type (e.g.
                      `(+)` in `Array.map2 (+) l1 l2`, for `arithm of
                      array`): same as [`DirectX] for an applied call. *)
                   (match Types.get_desc (Ctype.expand_head env self_ty) with
                    | Tconstr (path, [], _) ->
                      (match path_is_x_field path with
                       | Some field ->
                         record_entry (loc_key e.exp_loc) (ViaDictParam (dict_param field, name));
                         (match !current_impl_module with
                          | Some mod_name -> add_dict_requirement mod_name (field, trait_name)
                          | None -> ())
                       | None -> ())
                    | _ -> ()))
              | _ -> ()))
        | _ -> ())
     | _ -> ());
    (match !pending_check with
     | Some (m, key) when (not (Hashtbl.mem table key)) && not !self_hit ->
       if not (Hashtbl.mem pending_impls m) then begin
         Hashtbl.replace pending_impls m ();
         pending_log := m :: !pending_log
       end
     | _ -> ());
    pending_check := None;
    Tast_iterator.default_iterator.expr iter e
  in
  (* Tracks [current_impl_module] while walking into an `impl`'s module
     body (always a `Tstr_module` in this language -- nothing else produces
     one), so [expr] above can tell a self-reference (skip) from a call to
     some other, already-defined impl (rewrite normally). *)
  let module_binding (iter : Tast_iterator.iterator) (mb : Typedtree.module_binding) =
    let prev = !current_impl_module in
    current_impl_module := mb.mb_name.txt;
    Tast_iterator.default_iterator.module_binding iter mb;
    current_impl_module := prev
  in
  let iterator = { Tast_iterator.default_iterator with expr; module_binding } in
  (* Item by item: one the probe reused as is (the very same typed item,
     see [probe_typecheck]'s cache), with the same requirements as when it
     was last walked (they only grow: their count tells), gives the same
     results again, replayed instead of walked. *)
  if cache then Hashtbl.reset pending_impls;
  let req_count () =
    ( Hashtbl.fold (fun _ reqs n -> n + 1 + List.length reqs) dict_requirements 0,
      Hashtbl.length pending_impls + (1000 * !pending_version),
      !defer_to_pending )
  in
  let previous = !harvest_cache in
  let walked = ref [] in
  let next_cache =
    List.map
      (fun (ti : Typedtree.structure_item) ->
        let count = req_count () in
        match
          if cache then List.find_opt (fun (ti', count', _, _, _, _) -> ti' == ti && count' = count) previous else None
        with
        | Some ((_, _, entries, reqs, misses, pending) as cached) ->
          List.iter (fun (key, target) -> Hashtbl.replace table key target) (List.rev entries);
          List.iter (fun (m, req) -> add_dict_requirement m req) (List.rev reqs);
          overload_misses := misses @ !overload_misses;
          List.iter (fun m -> Hashtbl.replace pending_impls m ()) pending;
          cached
        | None ->
          entry_log := [];
          req_log := [];
          pending_log := [];
          let misses_before = !overload_misses in
          iterator.structure_item iterator ti;
          walked := loc_key ti.str_loc :: !walked;
          let rec added l = if l == misses_before then [] else match l with x :: rest -> x :: added rest | [] -> [] in
          (ti, count, !entry_log, !req_log, added !overload_misses, !pending_log))
      typed.str_items
  in
  if cache then begin
    harvest_cache := next_cache;
    harvest_walked := !walked
  end;
  (match List.rev !overload_misses with
   | (loc, trait_name, name, tys) :: _ ->
     (* Names the missing method too: a trait may be implemented only in
        part for some types (e.g. `indexable`'s `[]` but not `[]=` for a
        read-only `string`). *)
     let shown = match SCaml.Op_names.prettify name with p when p = name -> "`" ^ name ^ "`" | p -> p in
     let report =
       Location.error ~loc (Printf.sprintf "No impl of %s (trait `%s`) for %s" shown trait_name (String.concat ", " tys))
     in
     (* The last-resort pass may still get past it (see the pipeline). *)
     if !default_free_type_vars then raise (Overload_miss report);
     Location.print_report Format.err_formatter report;
     exit 1
   | [] -> ());
  (table, dict_requirements)

(* `let l = init 1 2` with `init`'s self type only in its result (`fn init
   of int -> int -> a`): OCaml would generalize that result (the relaxed
   value restriction), and the call's own type, by which it is dispatched
   (see [harvest_dispatch]), would never learn what `l` is used as. As
   `(fun l -> ...) (init 1 2)`, whose parameter stays monomorphic, it does
   (a `match` would generalize its scrutinee the same way). *)
let ungeneralize_self_results trait_methods (abstract_types : (string, string list) Hashtbl.t) (structure : Parsetree.structure) =
  let returning_self =
    List.filter_map
      (fun (name, trait, names, _) ->
        match List.rev names, Option.bind (Hashtbl.find_opt abstract_types trait) (fun ts -> List.nth_opt (List.rev ts) 0) with
        | ret :: (_ :: _ as params), Some self when ret = self && not (List.mem self params) -> Some name
        | _ -> None)
      trait_methods
  in
  let rec head (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_apply (f, _) -> head f
    | Pexp_ident { txt = Longident.Lident n; _ } -> Some n
    | _ -> None
  in
  let expr (m : Ast_mapper.mapper) (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_let (Nonrecursive, [ vb ], body)
      when (match vb.pvb_expr.pexp_desc with Pexp_apply _ -> true | _ -> false)
           && (match head vb.pvb_expr with Some n -> List.mem n returning_self | None -> false) ->
      let param = { Parsetree.pparam_loc = vb.pvb_pat.ppat_loc; pparam_desc = Pparam_val (Nolabel, None, m.pat m vb.pvb_pat) } in
      { e with
        pexp_desc =
          Pexp_apply
            (Ast_helper.Exp.function_ ~loc:e.pexp_loc [ param ] None (Pfunction_body (m.expr m body)),
             [ (Nolabel, m.expr m vb.pvb_expr) ]) }
    | _ -> Ast_mapper.default_mapper.expr m e
  in
  if returning_self = [] then structure
  else
    let mapper = { Ast_mapper.default_mapper with expr } in
    mapper.structure mapper structure

let fresh_dispatch_module_counter = ref 0

let fresh_dispatch_module_name () =
  incr fresh_dispatch_module_counter;
  Printf.sprintf "Dispatch_mod_%d" !fresh_dispatch_module_counter

(* Rewrites the original Parsetree: every bare identifier whose location was
   resolved by [harvest_dispatch] becomes a plain qualified reference (`+`
   -> `Arithm__int.(+)`), a direct projection of the enclosing generic
   impl's own functor parameter (`println` -> `X.println`, from [ViaDictParam]),
   or, when the impl is a functor (an "other" abstract type needs a
   concrete argument per call site, e.g. `iter` on an `int array`), a `let
   module` binding a fresh name to the functor applied either to a
   freshly-built `struct type <field> = <concrete-or-X-projection> end`, or
   (for a [DictModule] target) directly to an existing impl module, then
   referencing the method through it:
     `iter` -> `let module M = Iterable__array (struct type a = int end)
                in M.iter`.
   Everything else is passed through unchanged. *)
(* A string or a char shown inside a structure (a list's element, a tuple's
   component...) is quoted, as an OCaml literal: `[ "a"; "b" ]`, `Some 'c'`,
   while `println "a"` (or an impl's own `print "[ "`) prints it as is. So
   where its `showable` impl is passed to a container's impl, it is replaced
   by this one, built inline: only OCaml's own stdlib, so nothing the
   pruning of SCaml's stdlib could have dropped. *)
let quoted_showable loc name =
  let ty, fmt =
    if name = impl_module_name "showable" "string" then ("string", "%S")
    else if name = impl_module_name "showable" "char" then ("char", "%C")
    else ("", "")
  in
  if ty = "" then None
  else
    Some
      (Ast_helper.Mod.structure ~loc
         (Parse.implementation (Lexing.from_string
            (Printf.sprintf
               (* `println` not through `print`: as a trait call, the
                  dispatch would send it to the unquoted impl. *)
               "type a = %s\nlet print (x : a) = Printf.printf \"%s\" x\nlet println (x : a) = Printf.printf \"%s\\n\" x\n"
               ty fmt fmt))))

(* The module a dictionary stands for: an impl, applied to the impls of its
   own elements ([nested], quoted for a string or a char, see
   [quoted_showable]). *)
let rec mod_of_dict ~nested loc (Dict (name, args)) =
  match (if nested && args = [] then quoted_showable loc name else None) with
  | Some m -> m
  | None ->
    List.fold_left
      (fun acc a -> Ast_helper.Mod.apply ~loc acc (mod_of_dict ~nested:true loc a))
      (Ast_helper.Mod.ident ~loc (Location.mkloc (Longident.Lident name) loc))
      args

(* [only]: the items (by location) that can have something to rewrite,
   the others are kept as they are (the very same values). *)
let rewrite_dispatch ?only (table : (string * int * int, dispatch_target) Hashtbl.t) (structure : Parsetree.structure) =
  let expr (mapper : Ast_mapper.mapper) (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_ident { txt = Longident.Lident name; _ } ->
      (match Hashtbl.find_opt table (loc_key e.pexp_loc) with
       | Some (Plain mod_name) ->
         let qualified = Option.get (Longident.unflatten [ mod_name; name ]) in
         { e with pexp_desc = Pexp_ident (Location.mkloc qualified e.pexp_loc) }
       | Some (ViaDictParam (dict_mod, method_name)) ->
         let qualified = Option.get (Longident.unflatten [ dict_mod; method_name ]) in
         { e with pexp_desc = Pexp_ident (Location.mkloc qualified e.pexp_loc) }
       | Some (Overload fname) -> { e with pexp_desc = Pexp_ident (Location.mkloc (Longident.Lident fname) e.pexp_loc) }
       | Some (Absurd n) ->
         let loc = e.pexp_loc in
         let body = Ast_helper.Exp.assert_ ~loc (Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "false") loc) None) in
         Ast_helper.Exp.function_ ~loc
           (List.init n (fun _ -> { Parsetree.pparam_loc = loc; pparam_desc = Pparam_val (Nolabel, None, Ast_helper.Pat.any ~loc ()) }))
           None (Pfunction_body body)
       | Some (DictCall dict) ->
         let loc = e.pexp_loc in
         let fresh = fresh_dispatch_module_name () in
         Ast_helper.Exp.struct_item ~loc
           (Ast_helper.Str.module_ ~loc (Ast_helper.Mb.mk ~loc (Location.mkloc (Some fresh) loc) (mod_of_dict ~nested:false loc dict)))
           (Ast_helper.Exp.ident ~loc (Location.mkloc (Option.get (Longident.unflatten [ fresh; name ])) loc))
       | Some (Functored (mod_name, arg)) ->
         let loc = e.pexp_loc in
         let fresh = fresh_dispatch_module_name () in
         let functor_arg_mod =
           match arg with
           | DictModule dict ->
             mod_of_dict ~nested:true loc dict
           | FieldStruct fields ->
             Ast_helper.Mod.structure ~loc
               (List.map
                  (fun (field_name, cref) ->
                    let manifest =
                      match cref with
                      | Concrete type_name -> Ast_helper.Typ.constr ~loc (Location.mkloc (SCaml.Type_name.lid type_name) loc) []
                      | ViaX field ->
                        Ast_helper.Typ.constr ~loc
                          (Location.mkloc (Longident.Ldot (Location.mkloc (Longident.Lident "X") loc, Location.mkloc field loc)) loc)
                          []
                      | ConcreteTree tree -> core_of_tree loc tree
                      | ViaXTree tree ->
                        let rec core = function
                          | XField f ->
                            Ast_helper.Typ.constr ~loc
                              (Location.mkloc (Longident.Ldot (Location.mkloc (Longident.Lident "X") loc, Location.mkloc f loc)) loc)
                              []
                          | XCon (n, args) when String.length n > 5 && String.sub n 0 5 = "tuple" ->
                            Ast_helper.Typ.tuple ~loc (List.map (fun t -> (None, core t)) args)
                          | XCon (n, args) -> Ast_helper.Typ.constr ~loc (Location.mkloc (SCaml.Type_name.lid n) loc) (List.map core args)
                        in
                        core tree
                    in
                    Ast_helper.Str.type_ ~loc Asttypes.Recursive
                      [ Ast_helper.Type.mk ~loc ~manifest (Location.mkloc field_name loc) ])
                  fields)
         in
         let functor_app =
           Ast_helper.Mod.apply ~loc
             (Ast_helper.Mod.ident ~loc (Location.mkloc (Longident.Lident mod_name) loc))
             functor_arg_mod
         in
         Ast_helper.Exp.struct_item ~loc
           (Ast_helper.Str.module_ ~loc (Ast_helper.Mb.mk ~loc (Location.mkloc (Some fresh) loc) functor_app))
           (Ast_helper.Exp.ident ~loc (Location.mkloc (Option.get (Longident.unflatten [ fresh; name ])) loc))
       | None -> e)
    | _ -> Ast_mapper.default_mapper.expr mapper e
  in
  let mapper = { Ast_mapper.default_mapper with expr } in
  match only with
  | None -> mapper.structure mapper structure
  | Some only ->
    List.map
      (fun (item : Parsetree.structure_item) ->
        if Hashtbl.mem only (loc_key item.pstr_loc) then mapper.structure_item mapper item else item)
      structure

(* For every impl [harvest_dispatch] found needing a *value*, not just a
   type, from its functor's padding parameter (e.g. `showable of array`'s
   `print` recursing on elements, via [ViaDictParam]/[dict_requirements]):
   upgrades that impl's own functor signature from a bare `sig type <field>
   end` to the needed trait's module type directly (e.g. `showable`), and
   renames every `X.<field>` type reference inside its body to `X.<that
   trait's own self type name>` (e.g. `X.a`) to match what `X` now actually
   exposes -- this covers both the self type's own manifest (`type a =
   X.__elem0__ array` -> `type a = X.a array`) and any `X.__elem0__`
   [rewrite_dispatch] already wrote for a [ContainerX] call inside the same
   body (e.g. `iter`'s own dispatch). External call sites got the matching
   half of this already, in [harvest_dispatch]'s `Concrete` case: passing
   the concrete impl module itself (e.g. `Showable__int`) as the whole
   functor argument instead of an anonymous struct. *)
let upgrade_dict_impls ?only
    (dict_requirements : (string, (string * string) list) Hashtbl.t)
    (abstract_types : (string, string list) Hashtbl.t)
    (structure : Parsetree.structure) : Parsetree.structure =
  if Hashtbl.length dict_requirements = 0 then structure
  else
    let self_name_of trait_name =
      match Hashtbl.find_opt abstract_types trait_name with
      | Some all -> (match List.rev all with last :: _ -> last | [] -> "a")
      | None -> "a"
    in
    (* `X.<old_field>` -> `<new_mod>.<new_field>`. *)
    let rename_fields (renames : (string * (string * string)) list) =
      let typ (mapper : Ast_mapper.mapper) (t : Parsetree.core_type) =
        match t.ptyp_desc with
        | Ptyp_constr ({ txt = Longident.Ldot ({ txt = Longident.Lident "X"; _ }, { txt = f; _ }); loc }, args)
          when List.mem_assoc f renames ->
          let new_mod, new_field = List.assoc f renames in
          { t with
            ptyp_desc =
              Ptyp_constr
                (Location.mkloc (Longident.Ldot (Location.mkloc (Longident.Lident new_mod) loc, Location.mkloc new_field loc)) loc, args)
          }
        | _ -> Ast_mapper.default_mapper.typ mapper t
      in
      { Ast_mapper.default_mapper with typ }
    in
    let upgrade_item (item : Parsetree.structure_item) =
      match item.pstr_desc with
      | Pstr_module ({ pmb_name = { txt = Some mod_name; _ }; pmb_expr; _ } as mb) ->
        (match Hashtbl.find_opt dict_requirements mod_name, pmb_expr.pmod_desc with
         (* One dictionary per parameter, each its own functor parameter,
            like the tuple impls: `showable of (k, v) Hashtbl.t` becomes
            `functor (X__k : showable) (X__v : showable) -> ...`, its
            `X.k` now `X__k.a`. (Only once: [Pmty_signature] is the
            original, never upgraded, parameter.) *)
         | Some reqs, Pmod_functor (Named ({ txt = Some "X"; _ }, { pmty_desc = Pmty_signature _; _ }), body)
           when multi_complete mod_name reqs ->
           let loc = pmb_expr.pmod_loc in
           let params = Hashtbl.find impl_functor_params mod_name in
           let mapper =
             rename_fields (List.map (fun p -> (p, (multi_dict_param p, self_name_of (List.assoc p reqs)))) params)
           in
           let renamed_body = mapper.Ast_mapper.module_expr mapper body in
           let new_mod_expr =
             List.fold_right
               (fun p acc ->
                 Ast_helper.Mod.functor_ ~loc
                   (Named (Location.mkloc (Some (multi_dict_param p)) loc,
                           Ast_helper.Mty.ident ~loc (Location.mkloc (Longident.Lident (List.assoc p reqs)) loc)))
                   acc)
               params renamed_body
           in
           { item with pstr_desc = Pstr_module { mb with pmb_expr = new_mod_expr } }
         | Some reqs, Pmod_functor _ when is_multi_impl mod_name && not (multi_complete mod_name reqs) ->
           let p, t = List.hd reqs in
           raise (Location.Error (Location.error ~loc:pmb_expr.pmod_loc
             (Printf.sprintf "This impl uses trait `%s` on its parameter `%s` only: with several parameters, it must use one on each of them" t p)))
         | Some ((field, needed_trait) :: _), Pmod_functor (Named (x_name, _old_sig), body) ->
           let new_self = self_name_of needed_trait in
           let mapper = rename_fields [ (field, ("X", new_self)) ] in
           let renamed_body = mapper.Ast_mapper.module_expr mapper body in
           let new_sig =
             Ast_helper.Mty.ident ~loc:pmb_expr.pmod_loc (Location.mkloc (Longident.Lident needed_trait) pmb_expr.pmod_loc)
           in
           let new_mod_expr = { pmb_expr with pmod_desc = Pmod_functor (Named (x_name, new_sig), renamed_body) } in
           { item with pstr_desc = Pstr_module { mb with pmb_expr = new_mod_expr } }
         | _ -> item)
      | _ -> item
    in
    (* Not walked this round and already upgraded: nothing new. *)
    let already_upgraded (item : Parsetree.structure_item) =
      match item.pstr_desc with
      | Pstr_module { pmb_expr = { pmod_desc = Pmod_functor (Named (_, { pmty_desc = Pmty_ident _; _ }), _); _ }; _ } -> true
      | _ -> false
    in
    match only with
    | None -> List.map upgrade_item structure
    | Some only ->
      List.map
        (fun (item : Parsetree.structure_item) ->
          if Hashtbl.mem only (loc_key item.pstr_loc) || not (already_upgraded item) then upgrade_item item else item)
        structure

(* A literal value of one of the known concrete types, used as a probe
   stub's body when the trait signature declares that exact return type
   (e.g. `unit` for `println`), so the stub's inferred type actually matches
   instead of always looking like "same type as the first argument". *)
let literal_of_known_type loc name : Parsetree.expression option =
  match name with
  | "unit" -> Some (Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None)
  | "int" -> Some (Ast_helper.Exp.constant ~loc (Ast_helper.Const.int 0))
  | "float" -> Some (Ast_helper.Exp.constant ~loc (Ast_helper.Const.float "0."))
  | "bool" -> Some (Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "false") loc) None)
  | "string" -> Some (Ast_helper.Exp.constant ~loc (Ast_helper.Const.string ""))
  | "char" -> Some (Ast_helper.Exp.constant ~loc (Ast_helper.Const.char ' '))
  | "bytes" -> Some (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Ldot (Location.mknoloc (Longident.Lident "Bytes"), Location.mknoloc "empty")) loc))
  | _ -> None

let mkbool_lit loc b =
  Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident (if b then "true" else "false")) loc) None

(* `if true then v1 else (if true then v2 else v3) ...`: forces every
   variable in the list to share one type, without needing any of them to
   actually be booleans (the condition is a fixed `true`). *)
let unify_same_type loc = function
  | [] | [ _ ] -> None
  | first :: rest ->
    Some (List.fold_left (fun acc v -> Ast_helper.Exp.ifthenelse ~loc (mkbool_lit loc true) acc (Some v)) first rest)

let find_index pred lst =
  let rec go i = function [] -> None | x :: xs -> if pred x then Some i else go (i + 1) xs in
  go 0 lst

(* A stub matching a trait method's actual signature shape (`names`, e.g.
   ["a"; "a"; "bool"] for `a -> a -> bool`), not just its arity: positions
   sharing the same placeholder name (e.g. both `a`s) are forced, via
   [unify_same_type], to share a type variable in the stub too -- otherwise
   nothing would relate their inferred types during the probe typecheck,
   and a use like `n <= 0` would leave `n`'s type totally unconstrained
   instead of unifying it with `0`'s (both are the trait's `a`). The return
   position is a literal when it names a known concrete type (e.g. `()` for
   `println`, `false` for a comparison), or otherwise the parameter sharing
   its placeholder name (so the return type is unified with that group too),
   or the first parameter as a last-resort fallback. *)
let mk_probe_stub loc (names : string list) : Parsetree.expression =
  if List.length names <= 1 then Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
  else
    let arity = List.length names - 1 in
    let param_names = List.filteri (fun i _ -> i < arity) names in
    let return_name = List.nth names arity in
    let params = List.mapi (fun i _ -> Printf.sprintf "probe_arg%d" i) param_names in
    let var i = Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident (List.nth params i)) loc) in
    let groups = Hashtbl.create 8 in
    List.iteri
      (fun i n ->
        if n <> "_" && not (List.mem n known_type_names) then
          Hashtbl.replace groups n (i :: (try Hashtbl.find groups n with Not_found -> [])))
      param_names;
    let unify_exprs =
      Hashtbl.fold
        (fun _ idxs acc ->
          match unify_same_type loc (List.map var idxs) with
          | Some e ->
            Ast_helper.Exp.apply ~loc
              (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "ignore") loc))
              [ (Asttypes.Nolabel, e) ]
            :: acc
          | None -> acc)
        groups []
    in
    let return_expr =
      match literal_of_known_type loc return_name with
      | Some lit -> lit
      | None when return_name = "_" ->
        (* A compound return type (e.g. `b t`): nothing to tie it to, so
           leave it fully polymorphic; the next probe round, once this call
           is dispatched to a real impl, sees its actual type. *)
        Ast_helper.Exp.apply ~loc
          (Ast_helper.Exp.ident ~loc (Location.mkloc (Option.get (Longident.unflatten [ "Obj"; "magic" ])) loc))
          [ (Asttypes.Nolabel, Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None) ]
      | None ->
        (match find_index (fun n -> n = return_name) param_names with
         | Some i -> var i
         | None when params = [] -> Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
         | None ->
           (* A return type tied to no parameter (e.g. `mul`'s `c` in `a ->
              b -> c`): left free, the overload picked decides it. *)
           Ast_helper.Exp.apply ~loc
             (Ast_helper.Exp.ident ~loc (Location.mkloc (Option.get (Longident.unflatten [ "Obj"; "magic" ])) loc))
             [ (Asttypes.Nolabel, Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None) ])
    in
    let body = List.fold_right (fun u acc -> Ast_helper.Exp.sequence ~loc u acc) unify_exprs return_expr in
    List.fold_right
      (fun p acc ->
        Ast_helper.Exp.function_ ~loc
          [ { Parsetree.pparam_loc = loc;
              pparam_desc = Parsetree.Pparam_val (Asttypes.Nolabel, None, Ast_helper.Pat.var (Location.mkloc p loc)) } ]
          None (Parsetree.Pfunction_body acc))
      params body

let build_probe_prelude (methods : (string * string * string list * (string * int) option list) list) : Parsetree.structure =
  List.map
    (fun (name, _trait, names, _domains) ->
      Ast_helper.Str.value ~loc Asttypes.Nonrecursive
        [ Ast_helper.Vb.mk ~loc (Ast_helper.Pat.var ~loc (Location.mkloc name loc)) (mk_probe_stub loc names) ])
    methods

let entry_point : Parsetree.structure_item =
  Ast_helper.Str.eval ~loc
    (Ast_helper.Exp.apply ~loc
       (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "main") loc))
       [ (Asttypes.Nolabel,
          Ast_helper.Exp.construct ~loc
            (Location.mkloc (Longident.Lident "()") loc)
            None) ])

let clone_file ~file ~orig ~sig_text = Printf.sprintf "%s [%s : %s]" file orig sig_text

(* The call that made [monomorphize] create each clone, keyed by the clone's
   tagged file name ([clone_file]): an error inside a clone is the caller's
   fault (wrong types for that function), so it gets reported there. *)
let clone_call_site : (string, Location.t) Hashtbl.t = Hashtbl.create 16


(* Splits "X has type A but an expression was expected of type B ..." (with
   whitespace already collapsed) into (A, B). *)
let type_clash (text : string) =
  let find sub from =
    let n = String.length sub in
    let rec go i = if i + n > String.length text then None else if String.sub text i n = sub then Some i else go (i + 1) in
    go from
  in
  match find "has type " 0 with
  | None -> None
  | Some i ->
    let a_start = i + 9 in
    (match find " but an expression was expected of type " a_start with
     | None -> None
     | Some j ->
       let b_start = j + 40 in
       let b_end = match find " Type " b_start with Some k -> k | None -> String.length text in
       Some (String.sub text a_start (j - a_start), String.trim (String.sub text b_start (b_end - b_start))))

(* `+` for (int, float) returning float where `int -> float -> int` was
   expected: the classic fold whose accumulator starts with the wrong type
   (`sum 0 t` on a float btree). *)
let accumulator_hint (text : string) : string option =
  let arrows t = List.map String.trim (String.split_on_char '>' t) |> List.map (fun p -> if String.ends_with ~suffix:"-" p then String.trim (String.sub p 0 (String.length p - 1)) else p) in
  match String.index_opt text '`', type_clash text with
  | Some q, Some (got, expected) ->
    (match String.index_from_opt text (q + 1) '`', arrows got, arrows expected with
     | Some q', [ a; b; r ], [ a'; b'; r' ] when a = a' && b = b' && r <> r' && r' = a ->
       let sym = String.sub text (q + 1) (q' - q - 1) in
       let example = if r = "float" && a = "int" then " (e.g. `0.` instead of `0`)" else "" in
       let article t = match t.[0] with 'a' | 'e' | 'i' | 'o' | 'u' -> "an " ^ t | _ -> "a " ^ t in
       Some
         (Printf.sprintf
            "`%s` on (%s, %s) returns %s, but its result must stay %s here, like a fold's accumulator keeps the type of its starting value. Start from %s instead%s."
            sym a b (article r) (article a) (article r) example)
     | _ -> None)
  | _ -> None

let collapse_spaces s =
  String.split_on_char '\n' s |> String.concat " " |> String.split_on_char ' ' |> List.filter (( <> ) "") |> String.concat " "

(* Reports a type error like OCaml would, but readable for SCaml: internal
   operator names become their symbol (`op___0___` -> `+`), and an error
   inside a monomorphized clone (e.g. the stdlib's `sum` specialized for the
   caller's types) is reported at the user's call that caused it, with the
   clone's own location kept as a detail. *)
let report_type_error exn =
  match exn with
  | Env.Error (Lookup_error (loc, _, Unbound_value (Lident name, _)))
    when (match Hashtbl.find_opt trait_calls (loc_key loc) with Some (n, _, _, _) -> n = name | None -> false) ->
    (* An unresolved trait method call (see [trait_calls]), not a real
       unknown name: say which impl is missing. *)
    let _, trait_name, ty, kind = Hashtbl.find trait_calls (loc_key loc) in
    let shown = match SCaml.Op_names.prettify name with p when p = name -> "`" ^ name ^ "`" | p -> p in
    let main, sub =
      match kind with
      | Unknown ->
        ( Printf.sprintf "Can't tell which impl of %s (trait `%s`) to use: the type of its argument is unknown" shown trait_name,
          [ Location.msg "Hint: add a type annotation to fix it" ] )
      | Function ->
        ( Printf.sprintf "No impl of %s (trait `%s`) for a function (%s)" shown trait_name ty,
          [ Location.msg "Hint: is an argument missing in this call?" ] )
      | Other -> (Printf.sprintf "No impl of %s (trait `%s`) for %s" shown trait_name ty, [])
    in
    Location.print_report Format.err_formatter { (Location.error ~loc main) with sub }
  | _ ->
  match Location.error_of_exn exn with
  | Some (`Ok report) ->
    let render (m : Location.msg) = SCaml.Op_names.prettify (Format_doc.asprintf "%a" Format_doc.pp_doc m.txt) in
    let pretty (m : Location.msg) : Location.msg = { m with txt = Format_doc.doc_printf "%s" (render m) } in
    let main_loc = report.main.loc in
    let rec user_site (loc : Location.t) depth =
      match Hashtbl.find_opt clone_call_site loc.loc_start.pos_fname with
      | Some call when depth < 32 -> user_site call (depth + 1)
      | _ -> loc
    in
    let report =
      match Hashtbl.find_opt clone_call_site main_loc.loc_start.pos_fname with
      | None -> { report with main = pretty report.main; sub = List.map pretty report.sub }
      | Some _ ->
        let fname = main_loc.loc_start.pos_fname in
        (* The first tag only: a clone made inside another one is tagged
           again (`f.scaml [construct : string] [get_trans : string ->
           _]`), and the error is reported at the outermost call. *)
        let instance =
          match String.index_opt fname '[' with
          | Some i ->
            let j = Option.value (String.index_from_opt fname i ']') ~default:(String.length fname) in
            String.sub fname (i + 1) (j - i - 1)
          | None -> fname
        in
        let fn_name, sig_text =
          match String.index_opt instance ':' with
          | Some i -> String.trim (String.sub instance 0 i), String.trim (String.sub instance (i + 1) (String.length instance - i - 1))
          | None -> instance, ""
        in
        (* The clone's location, shown under its real file name. *)
        let untag (p : Lexing.position) =
          match String.index_opt p.pos_fname '[' with
          | Some i -> { p with pos_fname = String.trim (String.sub p.pos_fname 0 i) }
          | None -> p
        in
        let clone_loc = { main_loc with loc_start = untag main_loc.loc_start; loc_end = untag main_loc.loc_end } in
        let text = render report.main in
        let hint = match accumulator_hint (collapse_spaces text) with Some h -> [ Location.msg "Hint: %s" h ] | None -> [] in
        { report with
          main = Location.msg ~loc:(user_site main_loc 0) "This call to `%s` does not type-check: its arguments have types %s" fn_name sig_text;
          sub = (Location.msg ~loc:clone_loc "in `%s`: %s" fn_name text :: List.map pretty report.sub) @ hint }
    in
    Location.print_report Format.err_formatter report
  | _ -> Location.report_exception Format.err_formatter exn

(* Runs the probe typecheck and hands back the resulting Typedtree.structure,
   to be walked by [harvest_dispatch]. *)
(* `SCAML_TIME=1 scamlc f.scaml`: CPU time spent in each phase, on stderr. *)
let timing = Sys.getenv_opt "SCAML_TIME" <> None
let phase_times : (string, float * int) Hashtbl.t = Hashtbl.create 16
let phase_order = ref []
let timed name f =
  if not timing then f ()
  else begin
    let t0 = Sys.time () in
    let r = f () in
    let dt = Sys.time () -. t0 in
    (match Hashtbl.find_opt phase_times name with
     | Some (t, n) -> Hashtbl.replace phase_times name (t +. dt, n + 1)
     | None -> phase_order := name :: !phase_order; Hashtbl.replace phase_times name (dt, 1));
    r
  end
let report_times () =
  if timing then
    List.iter (fun name ->
      let t, n = Hashtbl.find phase_times name in
      Printf.eprintf "%-28s %7.3fs  x%d\n%!" name t n)
      (List.rev !phase_order)

(* The probe's items as typed by the last [probe_typecheck], each with its
   signature and the environment right after it, and the environment they
   started from. Between two rounds only a few items change (a call
   rewritten to its impl, a clone inserted), so the next probe types only
   those again and reuses the rest -- typing item by item, as the toplevel
   does. An item is only reused when typing it again could give nothing
   else, i.e. when everything before it is unchanged, or changed in a way
   that no later item can see:
   - an item rewritten without its signature changing (a function, or an
     impl's module): typed again, and the later items keep seeing the old
     one, which declares the same; a value's bound [Ident.t]s are renamed
     back to the old ones, so the later items' references to them (which
     [monomorphize] and [used_toplevel_bindings] match by [Ident.t]) still
     hold;
   - an item inserted that only binds new names (a clone): later items
     can't refer to it without having changed themselves.
   - an item removed that bound only values no later item mentions.
   Anything else (a signature changed, ...) types everything after it
   again. *)
let probe_cache : (Env.t * (Parsetree.structure_item * Typedtree.structure_item list * Types.signature * Env.t) list) option ref =
  ref None

(* The values a signature binds, if it binds nothing else. *)
let sig_values (sg : Types.signature) =
  let values = List.filter_map (function Types.Sig_value (id, vd, _) -> Some (id, vd) | _ -> None) sg in
  if List.length values = List.length sg then Some values else None

(* [new_sg] declares the same things as [old_sg], with the same types: the
   pairs of the values' [Ident.t]s, new to old (none for a module, a type,
   ...: nothing matches those by [Ident.t]). *)
let same_signature (new_sg : Types.signature) (old_sg : Types.signature) =
  let text sg = Format.asprintf "%a" Printtyp.signature sg in
  if text new_sg <> text old_sg then None
  else
    match sig_values new_sg, sig_values old_sg with
    | Some nv, Some ov when List.length nv = List.length ov -> Some (List.map2 (fun (n, _) (o, _) -> (n, o)) nv ov)
    | _ -> Some []

(* [items] with the [Ident.t]s of [pairs] renamed, where bound and used. *)
let rename_idents (pairs : (Ident.t * Ident.t) list) (items : Typedtree.structure_item list) =
  let find id = List.find_map (fun (a, b) -> if Ident.same a id then Some b else None) pairs in
  let pat : type k. Tast_mapper.mapper -> k Typedtree.general_pattern -> k Typedtree.general_pattern =
    fun m p ->
      let p = Tast_mapper.default.pat m p in
      match p.pat_desc with
      | Tpat_var (id, n, uid) ->
        (match find id with Some id' -> { p with pat_desc = Tpat_var (id', n, uid) } | None -> p)
      | _ -> p
  in
  let expr (m : Tast_mapper.mapper) (e : Typedtree.expression) =
    let e = Tast_mapper.default.expr m e in
    match e.exp_desc with
    | Texp_ident (Path.Pident id, lid, vd) ->
      (match find id with Some id' -> { e with exp_desc = Texp_ident (Path.Pident id', lid, vd) } | None -> e)
    | _ -> e
  in
  let m = { Tast_mapper.default with pat; expr } in
  List.map (m.structure_item m) items

(* Only binds values that [env] doesn't have yet. *)
let binds_new_values env (sg : Types.signature) =
  match sig_values sg with
  | Some values ->
    List.for_all
      (fun (id, _) ->
        match Env.find_value_by_name (Longident.Lident (Ident.name id)) env with
        | _ -> false
        | exception Not_found -> true)
      values
  | None -> false

(* Set by the rounds for the length of one probe: dispatches what an item's
   typing just resolved, and gives the rewritten item back if that changed
   it (see [type_refined] in [probe_typecheck]). *)
let probe_refine : (Parsetree.structure_item -> Typedtree.structure_item list -> Parsetree.structure_item option) option ref =
  ref None

(* The user structure as the last probe typed it, refined items included. *)
let probe_refined : Parsetree.structure ref = ref []

let probe_typecheck env (methods : (string * string * string list * (string * int) option list) list) (user_structure : Parsetree.structure) : Typedtree.structure =
  let probe_structure = build_probe_prelude methods @ user_structure @ [ entry_point ] in
  let fail exn =
    (* The last-resort pass handles its own failure (falls back). *)
    if !default_free_type_vars then raise exn;
    report_type_error exn;
    exit 1
  in
  if !default_free_type_vars then begin
    (* The last-resort rounds read types differently (see [defaultable]):
       typed from scratch, and not cached. *)
    probe_cache := None;
    match Typemod.type_structure env probe_structure with
    | (typedtree, _, _, _, _) -> typedtree
    | exception exn -> fail exn
  end else begin
    let cached = match !probe_cache with Some (env0, items) when env0 == env -> items | _ -> [] in
    let type_item env item =
      match Typemod.type_structure env [ item ] with
      | (t, sg, _, _, env') -> (t.Typedtree.str_items, sg, env')
      | exception exn -> fail exn
    in
    (* Typed, then dispatched and typed again for as long as that changes it:
       a chain of trait calls (`a + b + c`, each one's type known only once
       the one inside is dispatched) resolves here instead of over as many
       rounds. A retyping that fails keeps the last version, the rounds go on
       from it. *)
    let type_refined env item =
      let rec loop item ti sg env' fuel =
        match !probe_refine with
        | Some refine when fuel > 0 ->
          (match refine item ti with
           | Some item' ->
             (match Typemod.type_structure env [ item' ] with
              | (t, sg', _, _, env'') -> loop item' t.Typedtree.str_items sg' env'' (fuel - 1)
              | exception _ -> (item, ti, sg, env'))
           | None -> (item, ti, sg, env'))
        | _ -> (item, ti, sg, env')
      in
      let ti, sg, env' = type_item env item in
      loop item ti sg env' 64
    in
    (* [exact]: [env] is the very environment the cached items were typed
       in, so a reused item's own environment after it is right too; once
       an item was inserted, it's only equivalent, and the reused item's
       signature is added to it instead. *)
    (* For each name, the last item that mentions it: an item removed can
       be skipped over if no item from there on mentions what it bound. *)
    let last_mention = Hashtbl.create 256 in
    (* Per item, every name it mentions: a value, or a module through a
       qualified name. *)
    let mentions = Array.make (List.length probe_structure) [] in
    List.iteri
      (fun i item ->
        let add name =
          Hashtbl.replace last_mention name i;
          mentions.(i) <- name :: mentions.(i)
        in
        let add_head lid = match Longident.flatten lid with head :: _ -> add head | [] -> () in
        let expr (it : Ast_iterator.iterator) (e : Parsetree.expression) =
          (match e.pexp_desc with
           | Pexp_ident { txt = Longident.Lident name; _ } -> add name
           | Pexp_ident { txt = Longident.Ldot (m, _); _ } -> add_head m.txt
           | _ -> ());
          Ast_iterator.default_iterator.expr it e
        in
        let module_expr (it : Ast_iterator.iterator) (me : Parsetree.module_expr) =
          (match me.pmod_desc with Pmod_ident { txt; _ } -> add_head txt | _ -> ());
          Ast_iterator.default_iterator.module_expr it me
        in
        let it = { Ast_iterator.default_iterator with expr; module_expr } in
        it.structure_item it item)
      probe_structure;
    (* The names whose signature changed during this pass: an item that
       mentions one is typed again, even if it didn't change itself. *)
    let dirty = Hashtbl.create 16 in
    let mentions_dirty i = Hashtbl.length dirty > 0 && List.exists (Hashtbl.mem dirty) mentions.(i) in
    let names_of (sg : Types.signature) = List.map (fun it -> Ident.name (Types.signature_item_id it)) sg in
    (* [sg] declares the same names as [old_sg]: the same item, changed. *)
    let replaces (sg : Types.signature) (old_sg : Types.signature) =
      let names = names_of sg in
      names <> [] && List.sort compare names = List.sort compare (names_of old_sg)
    in
    let removable i (sg : Types.signature) =
      match sig_values sg with
      | Some values ->
        List.for_all
          (fun (id, _) -> match Hashtbl.find_opt last_mention (Ident.name id) with Some j -> j < i | None -> true)
          values
      | None -> false
    in
    let same (item' : Parsetree.structure_item) item = item' == item || item' = item in
    let rec reappears item cached n =
      n > 0 && (match cached with (item', _, _, _) :: rest -> same item' item || reappears item rest (n - 1) | [] -> false)
    in
    let rec go cached items i env exact acc =
      match items with
      | [] -> (acc, env)
      | item :: items' ->
        (match cached with
         | (item', ti, sg, env') :: cached' when same item' item && not (mentions_dirty i) ->
           let env_next = if exact then env' else Env.add_signature sg env in
           go cached' items' (i + 1) env_next exact ((item, ti, sg, env_next) :: acc)
         (* the cached item was removed (e.g. by [drop_unused_generics]),
            maybe with a few more after it *)
         | (_, _, old_sg, _) :: cached' when removable i old_sg && reappears item cached' 64 ->
           go cached' items i env false acc
         | (_, _, old_sg, old_env') :: cached' ->
           let item, ti, sg, env' = type_refined env item in
           (match same_signature sg old_sg with
            | Some pairs ->
              let env_next = if exact then old_env' else Env.add_signature old_sg env in
              go cached' items' (i + 1) env_next exact ((item, rename_idents pairs ti, old_sg, env_next) :: acc)
            | None when replaces sg old_sg ->
              (* e.g. a clone whose parameters got more precise: only what
                 mentions it needs typing again *)
              List.iter (fun n -> Hashtbl.replace dirty n ()) (names_of sg);
              go cached' items' (i + 1) env' false ((item, ti, sg, env') :: acc)
            | None ->
              if binds_new_values env sg then go cached items' (i + 1) env' false ((item, ti, sg, env') :: acc)
              else go [] items' (i + 1) env' false ((item, ti, sg, env') :: acc))
         | [] ->
           let item, ti, sg, env' = type_refined env item in
           go [] items' (i + 1) env' false ((item, ti, sg, env') :: acc))
    in
    let typed, final_env = go cached probe_structure 0 env true [] in
    let entries = List.rev typed in
    probe_cache := Some (env, entries);
    (* without the prelude and the entry point *)
    let first = List.length probe_structure - List.length user_structure - 1 in
    probe_refined :=
      List.filteri (fun i _ -> i >= first && i < List.length entries - 1) (List.map (fun (item, _, _, _) -> item) entries);
    { Typedtree.str_items = List.concat_map (fun (_, ti, _, _) -> ti) entries;
      str_type = List.concat_map (fun (_, _, sg, _) -> sg) entries;
      str_final_env = final_env }
  end

(* ---- Monomorphization ----

   Dispatch happens where a function is *defined*: a top-level function
   whose own parameters stay polymorphic (e.g. `fn tim2 m { map (..) m }`,
   or `fn show x { println x }`) has trait calls nothing inside it can
   resolve. So, once the probe rounds stop making progress, each such
   generic function gets one clone per concrete type it is called at, with
   its parameters annotated with that type:

     let tim2__mono1 (m : int array) = map (fun x -> x * 2) m

   and every such call site is pointed at its clone. The next probe rounds
   then resolve the clone's trait calls like any other code. Generic
   originals that end up unused are dropped ([drop_unused_generics]). *)

(* A top-level, single-name value binding: its name and expression. *)
let simple_binding (item : Parsetree.structure_item) =
  match item.pstr_desc with
  | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; pvb_expr; _ } ]) -> Some (txt, pvb_expr)
  | _ -> None

(* Does [e] still mention a trait method by its bare name, i.e. a call the
   dispatch couldn't resolve? *)
let has_unresolved_trait_call (method_names : string list) (e : Parsetree.expression) =
  let found = ref false in
  let expr self (e : Parsetree.expression) =
    (match e.pexp_desc with
     | Pexp_ident { txt = Longident.Lident n; _ } when List.mem n method_names -> found := true
     | _ -> ());
    Ast_iterator.default_iterator.expr self e
  in
  let it = { Ast_iterator.default_iterator with expr } in
  it.expr it e;
  !found

(* Top-level functions that are generic in the dispatch sense: they still
   hold an unresolved trait call, or call (or pass along) another such
   function -- `fn showall m { show m }` is as stuck as `show` itself. *)
let generic_functions (method_names : string list) (structure : Parsetree.structure) : string list =
  let fns = List.filter_map simple_binding structure in
  let rec fix acc =
    let acc' =
      List.filter_map
        (fun (name, e) ->
          if List.mem name acc then Some name
          else if has_unresolved_trait_call (method_names @ acc) e then Some name
          else None)
        fns
    in
    if List.length acc' = List.length acc then acc else fix acc'
  in
  fix []

(* How many parameters [e] takes up front (`fun a -> fun b -> ..` is 2). *)
let rec fun_arity (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_function (params, _, Pfunction_body body) -> List.length params + fun_arity body
  | Pexp_function (params, _, Pfunction_cases _) -> List.length params + 1
  | _ -> 0

(* The first [n] parameter types of a (possibly instantiated) function type. *)
let rec arrow_params n (ty : Types.type_expr) =
  if n = 0 then []
  else
    match Types.get_desc ty with
    | Tarrow (_, t1, t2, _) -> strip_tpoly t1 :: arrow_params (n - 1) t2
    | _ -> []

(* A fully known type, back as source syntax for an annotation; None if it
   still contains a type variable (or something we don't print).
   Abbreviations are expanded first, so equal types print equally
   (`Arithm__int.a Mappable__array.t` is just `int array`) and share one
   clone. *)
let rec core_of_type (env : Env.t) (ty : Types.type_expr) : Parsetree.core_type option =
  let core_of_type = core_of_type env in
  let all l = if List.for_all Option.is_some l then Some (List.map Option.get l) else None in
  match Types.get_desc (Ctype.expand_head env ty) with
  | Tpoly (t, []) -> core_of_type t
  | Tarrow (Nolabel, t1, t2, _) ->
    (match core_of_type t1, core_of_type t2 with
     | Some a, Some b -> Some (Ast_helper.Typ.arrow ~loc Nolabel a b)
     | _ -> None)
  | Ttuple l ->
    Option.map
      (fun cts -> Ast_helper.Typ.tuple ~loc (List.map2 (fun (lbl, _) ct -> (lbl, ct)) l cts))
      (all (List.map (fun (_, t) -> core_of_type t) l))
  | Tconstr (path, args, _) ->
    let name = Path.name path in
    if String.contains name '(' then None
    else
      (match Longident.unflatten (String.split_on_char '.' name), all (List.map core_of_type args) with
       | Some lid, Some cargs -> Some (Ast_helper.Typ.constr ~loc (Location.mkloc lid loc) cargs)
       | _ -> None)
  (* Same last-resort reading as [classify_texpr]'s: lets e.g. `length []`
     get a clone for `unit list`. *)
  | Tvar _ when defaultable (Ctype.expand_head env ty) ->
    if !absurd_free_type_vars then Some (Ast_helper.Typ.constr ~loc (Location.mkloc (Longident.Lident never_type_name) loc) [])
    else Some (Ast_helper.Typ.constr ~loc (Location.mkloc (Longident.Lident "unit") loc) [])
  | _ -> None

(* What [core_of_type] gives up on, read as far as it goes: a type
   constructor whose arguments are not all known, with `_` for those
   (`'a Stack.t` is `_ Stack.t`). That is enough for a clone to pick the
   impl of a trait call on it (`push x sd`, `sd` a `Stack.t`): the element
   type comes from the other arguments, once the clone's own calls are
   resolved. None for anything else (a bare type variable, an arrow...). *)
let partial_core_of_type (env : Env.t) (ty : Types.type_expr) : Parsetree.core_type option =
  match Types.get_desc (Ctype.expand_head env ty) with
  | Tconstr (path, args, _) when args <> [] ->
    let name = Path.name path in
    if String.contains name '(' then None
    else
      (match Longident.unflatten (String.split_on_char '.' name) with
       | Some lid ->
         let arg t = match core_of_type env t with Some c -> c | None -> Ast_helper.Typ.any ~loc () in
         Some (Ast_helper.Typ.constr ~loc (Location.mkloc lid loc) (List.map arg args))
       | None -> None)
  | _ -> None

(* [e] with its first parameters annotated with [tys], in order. *)
let annotate_params (tys : Parsetree.core_type list) (e : Parsetree.expression) =
  let rec go tys (e : Parsetree.expression) =
    match tys, e.pexp_desc with
    | [], _ -> e
    | _, Pexp_function (params, c, body) ->
      let rec ann tys = function
        | [] -> (tys, [])
        | (p : Parsetree.function_param) :: rest ->
          (match tys, p.pparam_desc with
           | ty :: tys', Pparam_val (lbl, def, pat) ->
             (* `?(x=0)`: the function's type takes an `int option`, but
                `x` itself is the `int`. *)
             let ty =
               match def, ty.Parsetree.ptyp_desc with
               | Some _, Ptyp_constr ({ txt; _ }, [ t ]) when Longident.last txt = "option" -> t
               | _ -> ty
             in
             let p' = { p with pparam_desc = Parsetree.Pparam_val (lbl, def, Ast_helper.Pat.constraint_ ~loc:pat.ppat_loc pat ty) } in
             let tys'', rest' = ann tys' rest in
             (tys'', p' :: rest')
           | _ ->
             let tys', rest' = ann tys rest in
             (tys', p :: rest'))
      in
      let tys', params' = ann tys params in
      let body' =
        match body, tys' with
        | Parsetree.Pfunction_body b, _ -> Parsetree.Pfunction_body (go tys' b)
        (* `function | p -> ..`: its argument is the next parameter *)
        | Pfunction_cases (cases, l, attrs), ty :: _ ->
          let annotate (c : Parsetree.case) = { c with pc_lhs = Ast_helper.Pat.constraint_ ~loc:c.pc_lhs.ppat_loc c.pc_lhs ty } in
          Pfunction_cases (List.map annotate cases, l, attrs)
        | cases, [] -> cases
      in
      { e with pexp_desc = Pexp_function (params', c, body') }
    | _ -> e
  in
  go tys e

(* A fresh copy of a generic function's definition for one concrete
   instantiation. Every location is moved to a distinct file name (which
   also names the instantiation, for error messages): dispatch results are
   keyed by source location, and two clones of the same body must not share
   them. Self-references are renamed too, so a recursive function recurses
   into its own clone. *)
let clone_loc ~orig ~sig_text (l : Location.t) =
  let tag (p : Lexing.position) = { p with pos_fname = clone_file ~file:p.pos_fname ~orig ~sig_text } in
  { l with loc_start = tag l.loc_start; loc_end = tag l.loc_end }

let mk_clone ~orig ~clone ~(tys : Parsetree.core_type list) ~(sig_text : string) (e : Parsetree.expression) =
  let location _ l = clone_loc ~orig ~sig_text l in
  let expr (m : Ast_mapper.mapper) (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_ident { txt = Longident.Lident n; loc } when n = orig ->
      { e with pexp_desc = Pexp_ident (Location.mkloc (Longident.Lident clone) (m.location m loc)); pexp_loc = m.location m e.pexp_loc }
    | _ -> Ast_mapper.default_mapper.expr m e
  in
  let mapper = { Ast_mapper.default_mapper with location; expr } in
  annotate_params tys (mapper.expr mapper e)

(* Names given by [monomorphize] to its clones (`tim2__mono1`). *)
let is_clone_name name =
  let rec go i = i + 6 <= String.length name && (String.sub name i 6 = "__mono" || go (i + 1)) in
  go 0

(* A clone made in an earlier step sits before its first user then, but a
   later clone may reuse it (the same function at the same types) from
   further up: `f__mono5`, placed before `Nn__linear`, calling `g__mono3`
   placed after it. Each such clone moves up to right before its first
   user, along with the clones in between that it needs, when nothing else
   it refers to is defined in between. *)
let hoist_clones (structure : Parsetree.structure) =
  let clone_name (item : Parsetree.structure_item) =
    match item.pstr_desc with
    | Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { txt; _ }; _ }; _ } ]) when is_clone_name txt -> Some txt
    | _ -> None
  in
  let uses name item = List.mem (name, None) (referenced item) in
  (* The items of [arr] from [j] to [i] (excluded) that [i] needs, directly
     or through each other, as indices; None if one isn't a clone. *)
  let needed arr j i =
    let rec go acc = function
      | [] -> Some acc
      | k :: todo ->
        let deps =
          List.filter
            (fun d -> d >= j && d < i && not (List.mem d acc) && List.exists (fun n -> uses n arr.(k)) (provided_names arr.(d)))
            (List.init (i - j) (fun d -> j + d))
        in
        if List.exists (fun d -> clone_name arr.(d) = None) deps then None else go (deps @ acc) (deps @ todo)
    in
    go [ i ] [ i ]
  in
  (* each move is up, and the clones' dependencies are acyclic: bounded
     anyway *)
  let rec pass fuel items =
    let arr = Array.of_list items in
    let n = Array.length arr in
    let rec find i =
      if i >= n then None
      else
        match clone_name arr.(i) with
        | Some name ->
          let rec first j = if j >= i then None else if uses name arr.(j) then Some j else first (j + 1) in
          (match first 0 with
           | Some j -> (match needed arr j i with Some block -> Some (j, block) | None -> find (i + 1))
           | None -> find (i + 1))
        | None -> find (i + 1)
    in
    match if fuel = 0 then None else find 0 with
    | None -> items
    | Some (j, block) ->
      let moved = List.filteri (fun k _ -> List.mem k block) items in
      pass (fuel - 1)
        (List.concat (List.mapi (fun k it -> if List.mem k block then [] else if k = j then moved @ [ it ] else [ it ]) items))
  in
  pass 1000 structure

(* One monomorphization step over a structure that the probe rounds can no
   longer improve ([typed] is that same structure's probe). [cache] maps
   (function, parameter types) to an existing clone's name across steps.
   Covers top-level functions and local ones (`let f = fun .. in ..`, e.g.
   a nested `fn aux acc n { .. }`): a local clone is bound right after its
   original, in the same scope. Bindings are matched between the Parsetree
   and the probe's Typedtree by their pattern's location. Returns the new
   structure and whether anything changed. *)
let monomorphize (method_names : string list) (cache : (string, string) Hashtbl.t) (counter : int ref)
    (typed : Typedtree.structure) (structure : Parsetree.structure) : Parsetree.structure * bool =
  let generic_names = generic_functions method_names structure in
  let stuck = method_names @ generic_names in
  (* Candidates, by pattern location: functions still holding an
     unresolved trait call (or, top-level, calling another such function). *)
  let candidates = Hashtbl.create 8 in
  let top_level = List.filter_map simple_binding structure |> List.map fst in
  (* A local function is one too when it calls another local one that is
     (`h` calling `g` in `fn g i { p[i] } fn h i { map (^e -> g i) l }`,
     once `map` is resolved): only a clone of `h` gives `g` its argument
     types. Until nothing changes. *)
  let local_generic = ref [] in
  (* Only a `let .. in` one: a clone of anything else local (an impl's
     method, in its `struct`) would have nowhere to go. *)
  let let_bound = Hashtbl.create 16 in
  let expr self (e : Parsetree.expression) =
    (match e.pexp_desc with
     | Pexp_let (_, vbs, _) ->
       List.iter
         (fun (vb : Parsetree.value_binding) ->
           match vb.pvb_pat.ppat_desc with Ppat_var { loc; _ } -> Hashtbl.replace let_bound (loc_key loc) () | _ -> ())
         vbs
     | _ -> ());
    Ast_iterator.default_iterator.expr self e
  in
  let it = { Ast_iterator.default_iterator with expr } in
  it.structure it structure;
  let rec find_candidates () =
    let before = Hashtbl.length candidates in
    let value_binding self (vb : Parsetree.value_binding) =
      (match vb.pvb_pat.ppat_desc with
       | Ppat_var { txt = name; loc = name_loc } when fun_arity vb.pvb_expr > 0 && not (name_loc = Location.none) ->
         let is_generic =
           if List.mem name top_level then List.mem name generic_names
           else
             (* not an impl's method: its clone would never be bound *)
             Hashtbl.mem let_bound (loc_key name_loc)
             && (has_unresolved_trait_call stuck vb.pvb_expr || has_unresolved_trait_call !local_generic vb.pvb_expr)
         in
         if is_generic then begin
           Hashtbl.replace candidates (loc_key name_loc) (fun_arity vb.pvb_expr);
           if Hashtbl.mem let_bound (loc_key name_loc) && not (List.mem name !local_generic) then
             local_generic := name :: !local_generic
         end
       | _ -> ());
      Ast_iterator.default_iterator.value_binding self vb
    in
    let it = { Ast_iterator.default_iterator with value_binding } in
    it.structure it structure;
    if Hashtbl.length candidates > before then find_candidates ()
  in
  find_candidates ();
  (* ...whose parameters really are polymorphic (otherwise a clone would be
     no more concrete than the original). Keyed by their own [Ident.t], so
     a local variable shadowing the name is never mistaken for them. *)
  let generic = ref [] in
  let value_binding (self : Tast_iterator.iterator) (vb : Typedtree.value_binding) =
    (match vb.vb_pat.pat_desc with
     | Tpat_var (id, { txt = name; loc = name_loc }, _) ->
       (match Hashtbl.find_opt candidates (loc_key name_loc) with
        | Some arity ->
          let params = arrow_params arity vb.vb_expr.exp_type in
          if List.exists (fun t -> core_of_type vb.vb_expr.exp_env t = None) params then
            generic := (id, (name, arity, loc_key name_loc)) :: !generic
        | None -> ())
     | _ -> ());
    Tast_iterator.default_iterator.value_binding self vb
  in
  let it = { Tast_iterator.default_iterator with value_binding } in
  it.structure it typed;
  if !generic = [] then (structure, false)
  else begin
    (* Call sites with fully known parameter types -> their clone. *)
    let redirect = Hashtbl.create 8 in
    let new_clones = ref [] in
    (* The generic functions whose own definition is being walked: a
       recursive call from inside one (`aux (n - 1) ..`) stays on it, since
       a clone is only bound after the original, out of its scope. *)
    let inside = ref [] in
    let expr (it : Tast_iterator.iterator) (e : Typedtree.expression) =
      (match e.exp_desc with
       | Texp_ident (Pident id, _, _) when not (List.exists (Ident.same id) !inside) ->
         (match List.find_opt (fun (gid, _) -> Ident.same gid id) !generic with
          | Some (_, (name, arity, def_key)) ->
            let param_types = arrow_params arity (Ctype.expand_head e.exp_env e.exp_type) in
            let params = List.map (core_of_type e.exp_env) param_types in
            (* Every parameter at least a known type constructor, some with
               unknown arguments (`_ Stack.t`, from `let s = make_stack
               ()`): enough for the clone to dispatch on, unlike a call
               with a parameter nothing is known of, which waits. *)
            let params =
              if List.for_all Option.is_some params || is_clone_name name then params
              else
                let partial =
                  List.map2
                    (fun c t -> match c with Some _ -> c | None -> partial_core_of_type e.exp_env t)
                    params param_types
                in
                if List.for_all Option.is_some partial && List.exists Option.is_none params then partial else params
            in
            (* A partly known call (`for [1; 2] (^i -> ..)`) is cloned too,
               its unknown parameters left as `_`: the callback's type only
               becomes known once the clone's own trait calls (`iter`) are
               resolved from the known ones. Never from a clone itself, so
               a clone that stays partly generic isn't re-cloned forever. *)
            (* Not from inside another generic function's own body: that one
               gets cloned first, and its clone knows more (`parse_tuple l`
               calling `split_apply_on_char f c l[1..2]`: `l` is only
               known in the clone). A clone made here, from what is
               still unknown, would stay stuck. *)
            let partial_ok = List.exists Option.is_some params && not (is_clone_name name) && !inside = [] in
            if List.length params = arity && (List.for_all Option.is_some params || partial_ok) then begin
              let tys = List.map (function Some t -> t | None -> Ast_helper.Typ.any ~loc ()) params in
              let sig_text = String.concat " -> " (List.map (Format.asprintf "%a" Pprintast.core_type) tys) in
              let (f, sc, ec) = def_key in
              let key = Printf.sprintf "%s@%s:%d-%d : %s" name f sc ec sig_text in
              let clone =
                match Hashtbl.find_opt cache key with
                | Some c -> c
                | None ->
                  incr counter;
                  let c = Printf.sprintf "%s__mono%d" name !counter in
                  Hashtbl.replace cache key c;
                  Hashtbl.replace clone_call_site (clone_file ~file:f ~orig:name ~sig_text) e.exp_loc;
                  new_clones := (def_key, name, c, tys, sig_text) :: !new_clones;
                  c
              in
              Hashtbl.replace redirect (loc_key e.exp_loc) clone
            end
          | None -> ())
       | _ -> ());
      Tast_iterator.default_iterator.expr it e
    in
    let value_binding (it : Tast_iterator.iterator) (vb : Typedtree.value_binding) =
      match vb.vb_pat.pat_desc with
      | Tpat_var (id, _, _) when List.exists (fun (gid, _) -> Ident.same gid id) !generic ->
        let saved = !inside in
        inside := id :: saved;
        Tast_iterator.default_iterator.value_binding it vb;
        inside := saved
      | _ -> Tast_iterator.default_iterator.value_binding it vb
    in
    let it = { Tast_iterator.default_iterator with expr; value_binding } in
    it.structure it typed;
    if Hashtbl.length redirect = 0 then (structure, false)
    else begin
      (* Clones of the binding at [pat_loc], built from its original
         expression as it was *before* redirecting, so the clone's own call
         sites get fresh locations and are redirected in a later step. *)
      let clones_of (pat : Parsetree.pattern) (e : Parsetree.expression) =
        let pat_loc = match pat.ppat_desc with Ppat_var { loc; _ } -> loc | _ -> pat.ppat_loc in
        List.filter_map
          (fun (def_key, orig, clone, tys, sig_text) ->
            if def_key <> loc_key pat_loc then None
            else
              Some
                (let ploc = clone_loc ~orig ~sig_text pat_loc in
                 Ast_helper.Vb.mk ~loc:ploc
                   (Ast_helper.Pat.var ~loc:ploc (Location.mkloc clone ploc))
                   (mk_clone ~orig ~clone ~tys ~sig_text e)))
          (List.rev !new_clones)
      in
      let expr (m : Ast_mapper.mapper) (e : Parsetree.expression) =
        match e.pexp_desc with
        | Pexp_ident { txt = Longident.Lident _; loc } when Hashtbl.mem redirect (loc_key e.pexp_loc) ->
          { e with pexp_desc = Pexp_ident (Location.mkloc (Longident.Lident (Hashtbl.find redirect (loc_key e.pexp_loc))) loc) }
        | Pexp_let (rf, [ vb ], body) ->
          let mapped = Ast_mapper.default_mapper.expr m e in
          (match clones_of vb.pvb_pat vb.pvb_expr, mapped.pexp_desc with
           | [], _ -> mapped
           | clones, Pexp_let (rf', vbs', body') ->
             let body'' = List.fold_right (fun c acc -> Ast_helper.Exp.let_ ~loc rf [ c ] acc) clones body' in
             { mapped with pexp_desc = Pexp_let (rf', vbs', body'') }
           | _ -> mapped)
        | _ -> Ast_mapper.default_mapper.expr m e
      in
      let mapper = { Ast_mapper.default_mapper with expr } in
      (* A top-level clone goes right before the first item that uses it,
         not right after its original: a clone is specialized to its
         caller's types (e.g. the stdlib's `sum` cloned for a user `tree`),
         which -- like the impls its body will dispatch to -- may only be
         declared after the original, but are always before that caller.
         Falls back to right after the original if nothing uses it. *)
      let mapped = List.map (fun item -> (item, mapper.structure_item mapper item)) structure in
      let uses name item' = List.mem (name, None) (referenced item') in
      let with_clones =
        let rec go before = function
          | [] -> []
          | ((item : Parsetree.structure_item), item') :: rest ->
            let clones =
              match item.pstr_desc with
              | Pstr_value (rf, [ vb ]) ->
                List.map
                  (fun (c : Parsetree.value_binding) ->
                    let name = match c.pvb_pat.ppat_desc with Ppat_var { txt; _ } -> txt | _ -> "" in
                    (name, Ast_helper.Str.value ~loc rf [ c ]))
                  (clones_of vb.pvb_pat vb.pvb_expr)
              | _ -> []
            in
            let placed, unused = List.partition (fun (name, _) -> List.exists (fun (_, i') -> uses name i') rest) clones in
            let pending = before @ placed in
            let here, later = List.partition (fun (name, _) -> uses name item') pending in
            List.map snd here @ (item' :: List.map snd unused) @ go later rest
        in
        go [] mapped
      in
      (hoist_clones with_clones, true)
    end
  end

(* Generic originals nothing refers to anymore (every use now goes to a
   clone): dropped, since their unresolved trait calls can't type-check.
   One still in use is kept, so the final type-check reports the real
   problem (its unresolved call) rather than a missing function. Local
   ones (`let f = .. in body`) likewise, when [body] no longer uses [f]. *)
(* The top-level bindings (by their pattern's location) that some *other*
   item really refers to, according to OCaml's own scoping on a probe
   typecheck: unlike comparing names, a local variable that merely shares a
   top-level function's name (the stdlib's `op |> f g { g f }` vs a user's
   `fn f`) doesn't count as a use of it. *)
let used_toplevel_bindings env methods (structure : Parsetree.structure) =
  let typed = probe_typecheck env methods structure in
  let owners = Ident.Tbl.create 256 in
  List.iteri
    (fun i (item : Typedtree.structure_item) ->
      match item.str_desc with
      | Tstr_value (_, vbs) ->
        List.iter
          (fun (vb : Typedtree.value_binding) ->
            (* Keyed by the bound name's own location: a pattern's location
               may well be `Location.none` (lib/parser.mly's [mkfn]). *)
            match vb.vb_pat.pat_desc with
            | Tpat_var (id, name, _) -> Ident.Tbl.replace owners id (i, loc_key name.loc)
            | _ -> ())
          vbs
      | _ -> ())
    typed.str_items;
  let used = Hashtbl.create 16 in
  List.iteri
    (fun i item ->
      let expr it (e : Typedtree.expression) =
        (match e.exp_desc with
         | Texp_ident (Path.Pident id, _, _) ->
           (match Ident.Tbl.find_opt owners id with
            | Some (j, key) when j <> i -> Hashtbl.replace used key ()
            | _ -> ())
         | _ -> ());
        Tast_iterator.default_iterator.expr it e
      in
      let it = { Tast_iterator.default_iterator with expr } in
      it.structure_item it item)
    typed.str_items;
  used

(* Whether [e] binds a clone of the local function [name] (`name__monoN`,
   see [monomorphize]). *)
let has_local_clone name (e : Parsetree.expression) =
  let prefix = name ^ "__mono" in
  let n = String.length prefix in
  let found = ref false in
  let pat self (p : Parsetree.pattern) =
    (match p.ppat_desc with
     | Ppat_var { txt; _ } when String.length txt > n && String.sub txt 0 n = prefix -> found := true
     | _ -> ());
    Ast_iterator.default_iterator.pat self p
  in
  let it = { Ast_iterator.default_iterator with pat } in
  it.expr it e;
  !found

let drop_unused_generics env methods (method_names : string list) (structure : Parsetree.structure) =
  let mentions name (e : Parsetree.expression) = has_unresolved_trait_call [ name ] e in
  let rec go structure =
    let generic_names = generic_functions method_names structure in
    let used = used_toplevel_bindings env methods structure in
    let droppable (item : Parsetree.structure_item) =
      match simple_binding item, item.pstr_desc with
      | Some (name, e), Pstr_value (_, [ { pvb_pat = { ppat_desc = Ppat_var { loc = name_loc; _ }; _ }; _ } ]) ->
        name <> "main" && fun_arity e > 0 && List.mem name generic_names
        && not (Hashtbl.mem used (loc_key name_loc))
      | _ -> false
    in
    let kept = List.filter (fun item -> not (droppable item)) structure in
    if List.length kept = List.length structure then structure else go kept
  in
  let structure = go structure in
  let stuck = method_names @ generic_functions method_names structure in
  (* Local functions still generic, also by calling one that is (`h`
     calling `g`, both left behind by their clones): until nothing
     changes. *)
  let local_stuck = ref [] in
  let rec find_local () =
    let before = List.length !local_stuck in
    let expr self (e : Parsetree.expression) =
      (match e.pexp_desc with
       | Pexp_let (_, vbs, _) ->
         List.iter
           (fun (vb : Parsetree.value_binding) ->
             match vb.pvb_pat.ppat_desc with
             | Ppat_var { txt = name; _ }
               when fun_arity vb.pvb_expr > 0 && not (List.mem name !local_stuck)
                    && has_unresolved_trait_call (stuck @ !local_stuck) vb.pvb_expr ->
               local_stuck := name :: !local_stuck
             | _ -> ())
           vbs
       | _ -> ());
      Ast_iterator.default_iterator.expr self e
    in
    let it = { Ast_iterator.default_iterator with expr } in
    it.structure it structure;
    if List.length !local_stuck > before then find_local ()
  in
  find_local ();
  let stuck = stuck @ !local_stuck in
  (* Innermost first: `h` (unused) goes before `g` is looked at, which is
     then unused too. *)
  let expr (m : Ast_mapper.mapper) (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_let (rf, [ ({ pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ }; pvb_expr; _ } as vb) ], body) ->
      let body' = m.expr m body in
      (* Or one whose clones took its place (`explore__mono3`), even if
         its own calls ended up resolved: unused, OCaml would warn. *)
      let cloned = has_local_clone name body' in
      if fun_arity pvb_expr > 0 && (has_unresolved_trait_call stuck pvb_expr || cloned) && not (mentions name body') then body'
      else { e with pexp_desc = Pexp_let (rf, [ m.value_binding m vb ], body') }
    | _ -> Ast_mapper.default_mapper.expr m e
  in
  let mapper = { Ast_mapper.default_mapper with expr } in
  mapper.structure mapper structure

(* A `fn` without parameters (marked by lib/parser.mly) must be a function,
   η-reduced (`fn triple { mul 3 }`): one whose body is a plain value (`fn
   answer { 6 * 7 }`) is either missing its `()` or a constant (#11). A
   type still unknown (a variable) may be a function: let through. *)
let check_eta (typed : Typedtree.structure) =
  let value_binding (it : Tast_iterator.iterator) (vb : Typedtree.value_binding) =
    if List.exists (fun (a : Parsetree.attribute) -> a.attr_name.txt = "scaml.eta") vb.vb_attributes then begin
      match Types.get_desc (Ctype.expand_head vb.vb_expr.exp_env vb.vb_expr.exp_type), vb.vb_pat.pat_desc with
      | (Tarrow _ | Tvar _), _ -> ()
      | _, Tpat_var (_, { txt = name; loc }, _) ->
        Location.print_report Format.err_formatter
          (Location.errorf ~loc
             "`%s` has no parameters, so it must be a function itself (η-reduced, as `fn triple { mul 3 }`), but its type is `%s`.@ \
              A function taking nothing takes `()`: `fn %s () { ... }`; a constant is a `let`: `let %s = ...;`"
             name (Format.asprintf "%a" Printtyp.type_expr vb.vb_expr.exp_type) name name);
        exit 1
      | _ -> ()
    end;
    Tast_iterator.default_iterator.value_binding it vb
  in
  let it = { Tast_iterator.default_iterator with value_binding } in
  it.structure it typed

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

(* Parses one .scaml source with our own lexer/parser, with the same error
   reporting as the top-level file (syntax errors via Location, lexing
   crashes dumping the tokens seen so far). Shared by the user's own file
   and every stdlib file [load_stdlib] pulls in below. *)
let parse_scaml_string (filename : string) (contents : string) : Parsetree.structure =
  let lexbuf = Lexing.from_string contents in
  Lexing.set_filename lexbuf filename;
  try SCaml.Parser.program SCaml.Lexer.token lexbuf
  with
  | SCaml.Parser.Error ->
    let err_loc =
      { Location.loc_start = lexbuf.Lexing.lex_start_p;
        loc_end = lexbuf.Lexing.lex_curr_p;
        loc_ghost = false }
    in
    let lexeme = Lexing.lexeme lexbuf in
    let what = if lexeme = "" then "end of file" else Printf.sprintf "%S" lexeme in
    Location.print_report Format.err_formatter
      (Location.error ~loc:err_loc (Printf.sprintf "Syntax error: unexpected %s" what));
    exit 1
  | SCaml.Lexer.Lex_error msg ->
    Printf.eprintf "-- tokens lexed before the crash --\n%!";
    SCaml.Token_debug.dump_until_crash filename contents;
    Printf.eprintf "%s: lexing error: %s\n%!" filename msg;
    exit 1
  | exn ->
    (* Covers `#use "path"` failures (bad path, or bad OCaml syntax). *)
    Location.report_exception Format.err_formatter exn;
    exit 1

let parse_scaml_file (filename : string) : Parsetree.structure =
  let contents =
    try read_file filename
    with Sys_error msg -> Printf.eprintf "%s\n%!" msg; exit 1
  in
  parse_scaml_string filename contents

(* stdlib/core.scaml is compiled into every program automatically, so its
   `fn`/`op`/`trait`/`impl` are always available with no explicit `#use`.
   The other stdlib files (e.g. stdlib/matrix.scaml) are opt-in, with
   `#use "stdlib/<name>.scaml"`. The files are embedded in the compiler at
   build time ([Stdlib_files], generated by bin/embed), so this works from
   any cwd or install location. *)
let load_stdlib () : Parsetree.structure =
  SCaml.Embedded.file := (fun path -> List.assoc_opt path Stdlib_files.files);
  SCaml.Embedded.parse_scaml := parse_scaml_string;
  let core = "stdlib/core.scaml" in
  parse_scaml_string core (List.assoc core Stdlib_files.files)

(* Overriding a stdlib function or operator is allowed, with a warning.

   Functions: the user's file is compiled in one structure with the stdlib,
   and the passes above (dead-code elimination, generics, monomorphization)
   tell top-level functions apart by name alone, so two `sum`s would get
   mixed up (failing far away, with an "Unbound value op___0___" inside the
   stdlib). The stdlib's own definition is therefore renamed (`sum` ->
   `sum__stdlib`), along with every use of it inside the stdlib, and in the
   user's file before the override (which, as in OCaml, still mean the
   stdlib's). Operators already get a fresh internal name per definition
   (`op___N___`), so they only get the warning.

   Returns both structures, renamed. *)
let warn_stdlib_overrides (stdlib : Parsetree.structure) (user : Parsetree.structure) =
  let bindings (item : Parsetree.structure_item) =
    match item.pstr_desc with
    | Pstr_value (_, vbs) ->
      List.filter_map
        (fun (vb : Parsetree.value_binding) ->
          match vb.pvb_pat.ppat_desc with
          | Ppat_var { txt; loc } -> Some (txt, loc)
          | _ -> None)
        vbs
    | _ -> []
  in
  let symbol_of fn = Hashtbl.find_opt SCaml.Op_names.symbol_of_fn fn in
  let warn loc what stdlib_loc =
    Location.print_report Format.err_formatter
      { Location.kind = Report_warning "stdlib-override";
        main = Location.msg ~loc "%s overrides the stdlib's definition" what;
        sub = [ Location.msg ~loc:stdlib_loc "the stdlib's definition is here" ];
        footnote = None }
  in
  (* The stdlib's operators, by symbol: top-level `op`s and trait ones. *)
  let stdlib_ops =
    List.concat_map
      (fun (item : Parsetree.structure_item) ->
        match item.pstr_desc with
        | Pstr_modtype { pmtd_type = Some { pmty_desc = Pmty_signature sigs; _ }; _ } ->
          List.filter_map
            (fun (s : Parsetree.signature_item) ->
              match s.psig_desc with
              | Psig_value vd ->
                (* A trait's generated signature may carry no location of its
                   own: point at the trait then. *)
                let loc = if vd.pval_loc = Location.none then item.pstr_loc else vd.pval_loc in
                Option.map (fun sym -> (sym, loc)) (symbol_of vd.pval_name.txt)
              | _ -> None)
            sigs
        | _ -> List.filter_map (fun (n, loc) -> Option.map (fun sym -> (sym, loc)) (symbol_of n)) (bindings item))
      stdlib
  in
  let stdlib_fns =
    List.concat_map bindings stdlib |> List.filter (fun (n, _) -> symbol_of n = None)
  in
  let rename_refs renames =
    let expr (m : Ast_mapper.mapper) (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_ident ({ txt = Longident.Lident n; _ } as lid) when List.mem_assoc n renames ->
        { e with pexp_desc = Pexp_ident { lid with txt = Longident.Lident (List.assoc n renames) } }
      | _ -> Ast_mapper.default_mapper.expr m e
    in
    let pat (m : Ast_mapper.mapper) (p : Parsetree.pattern) =
      match p.ppat_desc with
      | Ppat_var ({ txt = n; _ } as v) when List.mem_assoc n renames ->
        { p with ppat_desc = Ppat_var { v with txt = List.assoc n renames } }
      | _ -> Ast_mapper.default_mapper.pat m p
    in
    { Ast_mapper.default_mapper with expr; pat }
  in
  (* First pass: which stdlib names the user's file overrides, and at which
     item (the first definition wins; later ones are plain OCaml shadowing). *)
  let overrides = ref [] in
  List.iteri
    (fun i item ->
      List.iter
        (fun (n, loc) ->
          match symbol_of n with
          | Some sym ->
            (match List.assoc_opt sym stdlib_ops with
             | Some stdlib_loc -> warn loc (Printf.sprintf "Operator `%s`" sym) stdlib_loc
             | None -> ())
          | None ->
            (match List.assoc_opt n stdlib_fns with
             | Some stdlib_loc when not (List.mem_assoc n !overrides) ->
               warn loc (Printf.sprintf "`%s`" n) stdlib_loc;
               overrides := (n, i) :: !overrides
             | _ -> ()))
        (bindings item))
    user;
  let renames = List.map (fun (n, _) -> (n, n ^ "__stdlib")) !overrides in
  (* Second pass: in each user item, a name overridden only further down
     still means the stdlib's one, as does the overriding item's own name
     inside its own body unless it's recursive. *)
  let user' =
    List.mapi
      (fun i (item : Parsetree.structure_item) ->
        let is_rec = match item.pstr_desc with Pstr_value (Recursive, _) -> true | _ -> false in
        let still_stdlib =
          List.filter_map
            (fun (n, j) -> if j > i || (j = i && not is_rec) then Some (n, n ^ "__stdlib") else None)
            !overrides
        in
        if still_stdlib = [] then item
        else
          let m = rename_refs still_stdlib in
          match item.pstr_desc with
          (* Only the bodies: the names an item binds are the user's own. *)
          | Pstr_value (rf, vbs) ->
            let vbs = List.map (fun (vb : Parsetree.value_binding) -> { vb with pvb_expr = m.expr m vb.pvb_expr }) vbs in
            { item with pstr_desc = Pstr_value (rf, vbs) }
          | _ -> m.structure_item m item)
      user
  in
  let stdlib' = if renames = [] then stdlib else (rename_refs renames).structure (rename_refs renames) stdlib in
  (stdlib', user')

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  if List.mem "--version" args then begin
    Printf.printf "scamlc %s\n" Version.version;
    exit 0
  end;
  let verbose = List.mem "--verbose" args || List.mem "-v" args in
  let tokens_mode = List.mem "--tokens" args in
  (* Keep every intermediate file next to the source (.generated.ml, .cmi,
     .cmx, .o); by default only the executable is produced there. *)
  let keep = List.mem "--keep" args || List.mem "-k" args in
  (* Type the whole stdlib, not just what the program uses (see
     [prune_for_probe]): catches errors in its unused parts. *)
  let check_stdlib = List.mem "--check-stdlib" args in
  let positional = List.filter (fun a -> String.length a = 0 || a.[0] <> '-') args in
  match tokens_mode, positional with
  | true, [ filename ] ->
    let lexbuf = Lexing.from_string (read_file filename) in
    Lexing.set_filename lexbuf filename;
    (try SCaml.Token_debug.print_tokens lexbuf
     with SCaml.Lexer.Lex_error msg ->
       Printf.eprintf "%s: lexing error: %s\n" filename msg;
       exit 1)
  | false, [ filename ] ->
    (* Order matters and must be explicit: OCaml doesn't guarantee argument
       evaluation order, and `lib/parser.mly`'s operator_tbl/trait_def are
       global mutable state shared across parser calls in this process --
       stdlib must finish parsing (registering every trait's operators)
       *before* the user's own file is parsed, or the user's file's own
       `+`/`-`/... would fall back to plain OCaml's native (monomorphic)
       operators instead of the trait-dispatched ones. *)
    let stdlib_structure = timed "parse stdlib" (fun () -> with_tuple_impls (load_stdlib ())) in
    let user_file_structure = timed "parse user file" (fun () -> parse_scaml_file filename) in
    let stdlib_structure, user_file_structure = warn_stdlib_overrides stdlib_structure user_file_structure in
    let user_structure = stdlib_structure @ user_file_structure in
    let user_structure =
      if check_stdlib then user_structure
      else timed "prune for probe" (fun () -> prune_for_probe filename user_structure)
    in

    if not (has_main user_structure) then begin
      Printf.eprintf "%s: missing entry point (expected `fn main () { ... }`)\n" filename;
      exit 1
    end;
    (* `fn main { .. }`, without `()`, is no function (#11): its body would
       run as `main` is defined, and `main ()` fail far from it. *)
    List.iter
      (fun (item : Parsetree.structure_item) ->
        match item.pstr_desc with
        | Pstr_value (_, vbs) ->
          List.iter
            (fun (vb : Parsetree.value_binding) ->
              match vb.pvb_pat.ppat_desc, vb.pvb_expr.pexp_desc with
              | Ppat_var { txt = "main"; loc }, desc when (match desc with Pexp_function _ -> false | _ -> true) ->
                Location.print_report Format.err_formatter
                  (Location.error ~loc "`main` takes `()`: write `fn main () { ... }`");
                exit 1
              | _ -> ())
            vbs
        | _ -> ())
      user_structure;
    Clflags.include_dirs := "+str" :: !Clflags.include_dirs;
    Compmisc.init_path ();
    (* "unreachable case": only ever about [never_type_name], the empty type
       a call like `f None` is cloned with when nothing fixes the option's
       content (`Some a` can't happen there), never about the user's code.
       Off for ocamlopt too, below. *)
    ignore (Warnings.parse_options false "-56");
    let env = Compmisc.initial_env () in

    (* Probe typecheck + rewrite modules in ast + ocaml typecheck*)
    let trait_methods = collect_trait_methods user_structure in
    collect_user_type_names user_structure;
    (* The resolved structure, once it type-checked as a whole. *)
    let typechecked : Parsetree.structure option ref = ref None in
    let typed_final : Typedtree.structure option ref = ref None in
    let user_structure =
      try
      if trait_methods = [] then user_structure
      else begin
        let abstract_types = collect_trait_abstract_types user_structure in
        let user_structure = ungeneralize_self_results trait_methods abstract_types user_structure in
        let ctor_methods = collect_ctor_methods user_structure in
        let overloads = collect_overloads user_structure in
        (* Each round's rewrite gives the next probe real types for what it
           just dispatched (e.g. `map f m`'s result, an `int array` only
           once `map` is `Mappable__array.map`), which can unlock calls
           depending on it (e.g. `|> println`). Stops once a round changes
           nothing. *)
        let method_names = List.map (fun (name, _, _, _) -> name) trait_methods in
        let mono_cache = Hashtbl.create 16 in
        let mono_counter = ref 0 in
        (* Once a round changes nothing, monomorphize what's still stuck
           (see [monomorphize]) and go on: the clones need rounds of their
           own. Bounded, against e.g. polymorphic recursion cloning forever. *)
        (* The latest requirements, so a later pass over the result can go
           on from them instead of starting from scratch. *)
        let last_dict_requirements = ref (Hashtbl.create 16) in
        (* What a probe dispatches right away, item by item (see
           [probe_refine]): only plain renames to an impl or an overload.
           The others (a functor applied, a dictionary...) depend on what
           the whole round finds ([dict_requirements]), so they wait for it. *)
        let refine dict_requirements (item : Parsetree.structure_item) ti =
          match item.pstr_desc with
          | Pstr_value _ | Pstr_module _ ->
            let typed = { Typedtree.str_items = ti; str_type = []; str_final_env = env } in
            let table, _ = harvest_dispatch trait_methods abstract_types ctor_methods overloads dict_requirements typed in
            (* Only the calls still unresolved in [item] (a bare name): one
               dispatched by an earlier round (`Dispatch_mod_3.iter`) is
               found again, but has nothing left to rewrite, and mustn't
               hold the others back. *)
            let bare = Hashtbl.create 16 in
            let expr (it : Ast_iterator.iterator) (e : Parsetree.expression) =
              (match e.pexp_desc with
               | Pexp_ident { txt = Longident.Lident _; _ } -> Hashtbl.replace bare (loc_key e.pexp_loc) ()
               | _ -> ());
              Ast_iterator.default_iterator.expr it e
            in
            let it = { Ast_iterator.default_iterator with expr } in
            it.structure_item it item;
            Hashtbl.filter_map_inplace (fun key t -> if Hashtbl.mem bare key then Some t else None) table;
            let plain = Hashtbl.fold (fun _ t ok -> ok && (match t with Plain _ | Overload _ -> true | _ -> false)) table true in
            if Hashtbl.length table = 0 || not plain then None
            else (match rewrite_dispatch table [ item ] with [ item' ] when item' <> item -> Some item' | _ -> None)
          | _ -> None
        in
        (* Set when a round could only wait (see [pending_impls]): the next
           one doesn't. *)
        let skip_defer_next = ref false in
        let rec rounds n dict_requirements structure =
          let defer = not !skip_defer_next in
          skip_defer_next := false;
          if defer <> !defer_to_pending then (defer_to_pending := defer; incr pending_version);
          deferred_count := 0;
          let local = not !default_free_type_vars in
          if local then probe_refine := Some (refine dict_requirements);
          let typed_probe =
            Fun.protect ~finally:(fun () -> probe_refine := None)
              (fun () -> timed "probe typecheck" (fun () -> probe_typecheck env trait_methods structure))
          in
          let structure = if local then !probe_refined else structure in
          if !default_free_type_vars then protect_bound_vars typed_probe;
          let dispatch_table, dict_requirements =
            timed "harvest dispatch" (fun () -> harvest_dispatch ~cache:true trait_methods abstract_types ctor_methods overloads dict_requirements typed_probe)
          in
          last_dict_requirements := dict_requirements;
          let rewritten =
            let only = Hashtbl.create 64 in
            List.iter (fun key -> Hashtbl.replace only key ()) !harvest_walked;
            timed "rewrite dispatch" (fun () ->
              rewrite_dispatch ~only dispatch_table structure |> upgrade_dict_impls ~only dict_requirements abstract_types)
          in
          if n <= 1 then rewritten
          else if timed "compare structures" (fun () ->
                    (* the very same item, as most are: unchanged *)
                    List.compare_lengths rewritten structure <> 0
                    || List.exists2 (fun a b -> a != b && a <> b) rewritten structure)
          then rounds (n - 1) dict_requirements rewritten
          else
            match timed "monomorphize" (fun () -> monomorphize method_names mono_cache mono_counter typed_probe rewritten) with
            | mono, true -> rounds (n - 1) dict_requirements mono
            | _, false when !deferred_count > 0 && !defer_to_pending ->
              (* only waiting on impls that won't move any more: go on *)
              skip_defer_next := true;
              rounds (n - 1) dict_requirements rewritten
            | _, false -> rewritten
        in
        let resolved = rounds 32 (Hashtbl.create 16) user_structure in
        let resolved = timed "drop unused generics" (fun () -> drop_unused_generics env trait_methods method_names resolved) in
        let typechecks s =
          match Typemod.type_structure env (s @ [ entry_point ]) with
          | (typed, _, _, _, _) -> typechecked := Some s; typed_final := Some typed; true
          | exception _ -> false
        in
        if timed "check resolved" (fun () -> typechecks resolved) then resolved
        else begin
          (* Something is still unresolved, e.g. `println (last [])`, whose
             element type nothing fixes: retry with such free type variables
             read as `unit` (see [default_free_type_vars]). If that doesn't
             type-check either, the original error is the one reported. *)
          let retry ~absurd =
            default_free_type_vars := true;
            absurd_free_type_vars := absurd;
            let r =
              let start = if absurd then never_type_decl :: resolved else resolved in
              try Ok (rounds 32 (Hashtbl.copy !last_dict_requirements) start |> drop_unused_generics env trait_methods method_names)
              with exn -> Error exn
            in
            default_free_type_vars := false;
            absurd_free_type_vars := false;
            r
          in
          let defaulted = retry ~absurd:false in
          match defaulted with
          | Ok d when typechecks d -> d
          | _ ->
            (* Then with free type variables read as the empty type, which
               gets past trait calls that can never run (see
               [absurd_free_type_vars]). *)
            match retry ~absurd:true with
            | Ok a when typechecks a -> a
            | _ ->
              (* Even when it fails, the first retry got further (e.g. `last
                 []` is resolved), so its error is the one that points at
                 what's really left: report that one, from the final
                 typecheck below. *)
              match defaulted with
              | Ok d -> d
              | Error (Overload_miss report) ->
                Location.print_report Format.err_formatter report;
                exit 1
              | Error _ -> resolved
        end
      end
      (* An impl the dispatch can't build (see [upgrade_dict_impls]). *)
      with Location.Error _ as exn ->
        Location.report_exception Format.err_formatter exn;
        exit 1
    in
    let full_structure = (* prelude @ *) user_structure @ [ entry_point ] in

    (* Already done if it's the structure [typechecks] accepted. *)
    (match !typechecked with
     | Some s when s == user_structure -> ()
     | _ ->
       (try timed "final typecheck" (fun () ->
              let typed, _, _, _, _ = Typemod.type_structure env full_structure in
              typed_final := Some typed)
        with exn ->
          report_type_error exn;
          exit 1));
    Option.iter check_eta !typed_final;
    if verbose then print_endline "Typecheck OK";

    (* Only after the structure has been type-checked as a whole: the user's
       own file always in full, the stdlib only in what [prune_for_probe]
       kept (all of it with --check-stdlib). Pruning here only ever drops siblings nothing kept
       depends on, so it can't change whether what remains still type-checks. *)
    let full_structure = timed "dead code" (fun () -> eliminate_dead_code full_structure) in

    let ocaml_src = timed "print OCaml" (fun () -> Format.asprintf "%a" Pprintast.structure full_structure) in
    report_times ();
    if verbose then Printf.printf "Generated OCaml:\n%s\n" ocaml_src;

    let base = Filename.remove_extension filename in
    let exe_file = base ^ ".exe" in
    (* ocamlopt writes its .cmi/.cmx/.o next to the .ml it compiles: without
       --keep, that's a throwaway directory, removed afterwards. *)
    let build_dir = if keep then Filename.dirname filename else Filename.temp_dir "scaml" "" in
    let ml_file = Filename.concat build_dir (Filename.basename base ^ ".generated.ml") in
    let oc = open_out ml_file in
    output_string oc ocaml_src;
    close_out oc;

    let cmd =
      Printf.sprintf "ocamlfind ocamlopt -package str -linkpkg -w -56 %s -o %s"
        (Filename.quote ml_file) (Filename.quote exe_file)
    in
    let status = Sys.command cmd in
    if not keep then begin
      Array.iter (fun f -> Sys.remove (Filename.concat build_dir f)) (Sys.readdir build_dir);
      Sys.rmdir build_dir
    end;
    (match status with
     | 0 -> Printf.printf "Compiled -> %s\n" exe_file
     | code ->
       Printf.eprintf "ocamlfind ocamlopt failed (exit %d)\n" code;
       exit 1)
  | _ ->
    Printf.eprintf "Usage: %s [--tokens] [--verbose|-v] [--keep|-k] [--check-stdlib] <file.scaml>\n       %s --version\n" Sys.argv.(0) Sys.argv.(0);
    exit 1

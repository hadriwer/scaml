(* What an impl on a nested type targets (`impl conversionnable_matrix of
   e array array`, `impl showable of int array`): a pattern over types,
   whose leaves are the impl's parameters (the fields of its functor
   parameter) or concrete types. A call is dispatched to the most specific
   impl whose pattern its type matches (see bin/main.ml).

   An impl on a single level of type whose arguments are all parameters
   (`array`, `e array`, `(k, v) Hashtbl.t`) is the usual kind, named after
   its type alone and not recorded here. *)

type t =
  | Var of string                (* a parameter: any type *)
  | Con of string * t list       (* a type (encoded, see [Type_name]) applied to patterns *)

type impl = {
  pattern : t;
  module_name : string;
  params : string list;          (* the [Var]s, left to right: the functor's fields *)
}

(* Per trait, its impls on nested types, in the order they are defined. *)
let table : (string, impl) Hashtbl.t = Hashtbl.create 16

let register trait impl = Hashtbl.add table trait impl

let impls_of trait = List.rev (Hashtbl.find_all table trait)

(* How specific a pattern is: its number of types. `e array array` (2)
   wins over `e array` (1) for an `int array array`. *)
let rec specificity = function
  | Var _ -> 0
  | Con (_, args) -> 1 + List.fold_left (fun n p -> n + specificity p) 0 args

let rec vars = function
  | Var v -> [ v ]
  | Con (_, args) -> List.concat_map vars args

(* As written: `(int, v) Hashtbl.t`. *)
let rec to_string p =
  let name c = if Type_name.is_qualified c then String.concat "." (String.split_on_char '\'' c) else c in
  match p with
  | Var v -> v
  | Con (c, []) -> name c
  | Con (c, [ a ]) -> to_string a ^ " " ^ name c
  | Con (c, args) -> "(" ^ String.concat ", " (List.map to_string args) ^ ") " ^ name c

(* A unique module name for an impl on a nested type: the trait and the
   outermost type, as for any impl, plus a number. Never contains `__`
   (the separator of those names) past the trait's. *)
let counter = ref 0

let module_name trait head =
  incr counter;
  String.capitalize_ascii (Printf.sprintf "%s__%s'%d" trait head !counter)

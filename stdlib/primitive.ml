(* ARITHM *)
let scaml_add_int = ( + )
let scaml_min_int = ( - )
let scaml_tim_int = ( * )
let scaml_div_int = ( / )
let scaml_mod_int = (mod)

let scaml_add_float = ( +. )
let scaml_min_float = ( -. )
let scaml_tim_float = ( *. )
let scaml_div_float = ( /. )
let scaml_mod_float a b = (int_of_float a) mod (int_of_float b) |> float_of_int

let scaml_concat_string = ( ^ )
(* PRINT *)
let scaml_print_string = print_string
let scaml_print_newline = print_newline
let scaml_print_int = print_int
let scaml_print_float = print_float
let scaml_print_char = print_char

(* Comparable *)

let scaml_lt = (<)
let scaml_leq = (<=)
let scaml_gt = (>)
let scaml_geq = (>=)
let scaml_eq = (=)
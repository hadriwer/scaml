(****************************************************************************)
(*                                                                          *)
(*                                  SCaml                                   *)
(*                                                                          *)
(*   Hadrien Lagier (wer)                                                   *)
(*   Claude (Anthropic), co-author                                          *)
(*                                                                          *)
(*   Copyright (c) 2026 wer                                                 *)
(*                                                                          *)
(*   This file is distributed under the terms of the MIT License,           *)
(*   see the file LICENSE at the root of the project.                       *)
(*                                                                          *)
(****************************************************************************)

(* List *)
let scaml_list_cons x xs = x :: xs
let scaml_list_concat = (@)
(* CONVERSION *)
let scaml_int_of_float = int_of_float
let scaml_float_of_int = float_of_int
let scaml_int_of_string = int_of_string
let scaml_float_of_string = float_of_string
(* ARITHM *)
let scaml_neg_int = ( ~- )
let scaml_neg_float = ( ~-. )
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

(* RANGES: walked with OCaml's `for` on ints, `low`, `low + st`, ... (`st`
   already of the right sign, 0 for an empty range), up to `high`, included
   or not. *)
let scaml_range_count (low : int) (high : int) (inclusive : bool) (st : int) =
  if st > 0 then
    let bound = if inclusive then high + 1 else high in
    if bound <= low then 0 else (bound - low + st - 1) / st
  else if st < 0 then
    let bound = if inclusive then high - 1 else high in
    if bound >= low then 0 else (low - bound - st - 1) / (- st)
  else 0

let scaml_range_iteri f low high inclusive st =
  for k = 0 to scaml_range_count low high inclusive st - 1 do f k (low + k * st) done

let scaml_range_iter f low high inclusive st =
  for k = 0 to scaml_range_count low high inclusive st - 1 do f (low + k * st) done

let scaml_range_fold_left f acc low high inclusive st =
  let acc = ref acc in
  for k = 0 to scaml_range_count low high inclusive st - 1 do acc := f !acc (low + k * st) done;
  !acc

let scaml_range_fold_right f low high inclusive st acc =
  let acc = ref acc in
  for k = scaml_range_count low high inclusive st - 1 downto 0 do acc := f (low + k * st) !acc done;
  !acc

let scaml_range_forall p low high inclusive st =
  let n = scaml_range_count low high inclusive st in
  let rec go k = k >= n || (p (low + k * st) && go (k + 1)) in
  go 0

(* `#time let r = e;`: [f ()], printing how long it took (wall clock) on
   stderr, after flushing what the program printed so far. The heap is
   compacted first, outside of the time: what came before is collected,
   and the heap shrunk back, so that two `#time` in a row compare fairly
   (a heap already grown by the first made the second up to 15% faster;
   a full major collection alone doesn't shrink it). A debugging tool:
   that compaction has its cost. *)
let scaml_time what f =
  Stdlib.Gc.compact ();
  let t0 = Unix.gettimeofday () in
  let v = f () in
  let t = Unix.gettimeofday () -. t0 in
  let shown =
    if t >= 1. then Stdlib.Printf.sprintf "%.3f s" t
    else if t >= 1e-3 then Stdlib.Printf.sprintf "%.3f ms" (t *. 1e3)
    else if t >= 1e-6 then Stdlib.Printf.sprintf "%.3f µs" (t *. 1e6)
    else Stdlib.Printf.sprintf "%.0f ns" (t *. 1e9)
  in
  flush stdout;
  Stdlib.Printf.eprintf "time for computing %s : %s\n%!" what shown;
  v

(* Comparable *)
let scaml_compare = compare
let scaml_lt = (<)
let scaml_leq = (<=)
let scaml_gt = (>)
let scaml_geq = (>=)
let scaml_eq = (=)
let scaml_neq = (<>)

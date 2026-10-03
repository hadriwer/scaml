let scaml_add_int = (+)
let scaml_min_int = (-)
let scaml_tim_int = ( * )
let scaml_div_int = (/)
let scaml_mod_int = \#mod
let scaml_print_string = print_string
let scaml_print_newline = print_newline
let scaml_print_int = print_int
let scaml_lt = (<)
let scaml_leq = (<=)
let scaml_gt = (>)
let scaml_geq = (>=)
let scaml_eq = (=)
module type showable  =
  sig type a val print : a -> unit val println : a -> unit end
module Showable__string =
  struct type a = string
         let print (s : a) = scaml_print_string s end
module Showable__int =
  struct
    type a = int
    let print (i : a) = scaml_print_int i
    let println (i : a) = print i; scaml_print_newline ()
  end
module Showable__bool =
  struct
    type a = bool
    let print (b : a) =
      if b
      then Showable__string.print "true"
      else Showable__string.print "false"
    let println (b : a) = print b; print_newline ()
  end
module type arithm  =
  sig
    type a
    val op___0___ : a -> a -> a
    val op___1___ : a -> a -> a
    val op___2___ : a -> a -> a
    val op___3___ : a -> a -> a
    val op___4___ : a -> a -> a
  end
module Arithm__int =
  (struct
     type a = int
     let op___0___ = scaml_add_int
     let op___1___ = scaml_min_int
     let op___2___ = scaml_tim_int
     let op___3___ = scaml_div_int
     let op___4___ = scaml_mod_int
   end : arithm with type  a =  int)
module type comparable  =
  sig
    type a
    val op___5___ : a -> a -> bool
    val op___6___ : a -> a -> bool
    val op___7___ : a -> a -> bool
    val op___8___ : a -> a -> bool
    val op___9___ : a -> a -> bool
  end
module Comparable__int = struct let op___6___ = scaml_leq end
module Comparable__array(X:sig type __elem0__ end) =
  (struct
     type a = X.__elem0__ array
     let op___5___ = scaml_lt
     let op___6___ = scaml_leq
     let op___7___ = scaml_gt
     let op___8___ = scaml_geq
     let op___9___ = scaml_eq
   end : comparable with type  a =  X.__elem0__ array)
module type sumable  = sig type a type b val sum : a -> b -> a end
module Sumable__array(X:arithm) =
  (struct
     type a = X.a
     type b = X.a array
     let sum acc = fun (arr : b) -> ((Array.fold_left X.op___0___) acc) arr
   end : sumable with type  b =  X.a array and type  a =  X.a)
let fact_opt n =
  let rec aux acc =
    fun n ->
      if Comparable__int.op___6___ n 0
      then acc
      else (aux (Arithm__int.op___2___ n acc)) (Arithm__int.op___1___ n 1) in
  (aux 1) n
let main () =
  let t = (1, 2) in
  let a = (Array.init 10) fact_opt in
  let a' = (Array.init 10) fact_opt in
  Showable__int.println
    (((let module Dispatch_mod_3 = (Sumable__array)(Arithm__int) in
         Dispatch_mod_3.sum) 0) a);
  Showable__int.println (fst t);
  Showable__int.println (a.(0));
  Showable__int.println (fact_opt 24);
  Showable__bool.println
    ((let module Dispatch_mod_2 =
        (Comparable__array)(struct type __elem0__ = int end) in
        Dispatch_mod_2.op___9___) a a')
;;main ()
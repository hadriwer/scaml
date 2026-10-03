let scaml_add_int = (+)
let scaml_min_int = (-)
let scaml_tim_int = ( * )
let scaml_div_int = (/)
let scaml_mod_int = \#mod
let scaml_add_float = (+.)
let scaml_min_float = (-.)
let scaml_tim_float = ( *. )
let scaml_div_float = (/.)
let scaml_mod_float a b =
  ((int_of_float a) mod (int_of_float b)) |> float_of_int
let scaml_concat_string = (^)
let scaml_print_string = print_string
let scaml_print_newline = print_newline
let scaml_print_int = print_int
let scaml_print_float = print_float
let scaml_print_char = print_char
module type showable  =
  sig type a val print : a -> unit val println : a -> unit end
module Showable__string =
  (struct
     type a = string
     let print s = scaml_print_string s
     let println s = print s; scaml_print_newline ()
   end : showable with type  a =  string)
module Showable__int =
  (struct
     type a = int
     let print i = scaml_print_int i
     let println i = print i; scaml_print_newline ()
   end : showable with type  a =  int)
module Showable__float =
  (struct
     type a = float
     let print f = scaml_print_float f
     let println f = print f; scaml_print_newline ()
   end : showable with type  a =  float)
module Showable__char =
  (struct
     type a = char
     let print c = scaml_print_char c
     let println c = print c; scaml_print_newline ()
   end : showable with type  a =  char)
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
module Arithm__float =
  (struct
     type a = float
     let op___0___ = scaml_add_float
     let op___1___ = scaml_min_float
     let op___2___ = scaml_tim_float
     let op___3___ = scaml_div_float
     let op___4___ = scaml_mod_float
   end : arithm with type  a =  float)
let op___5___ f = fun g -> g f
let main () =
  Showable__int.println (Arithm__int.op___0___ 1 2);
  Showable__float.println (Arithm__float.op___0___ 1. 2.)
;;main ()
(** Support routines for the Idris 2 OCaml backend *)

type ival =  I of int | D of float | C of char | S of string | CON of con 
and con = { tag : int; vals : ival array };;

let idris_putStr str = print_string str

(* End of support library *)


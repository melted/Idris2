(** Support routines for the Idris 2 OCaml backend *)

type ival =  I of int | BI of Z.t | D of float | C of char | S of string | CON of con | NCON of ncon 
and con = { tag : int; vals : ival array }
and ncon = { name : string; args : ival array };;

let idris_putStr str = print_string str

(* End of support library *)


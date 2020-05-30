(* Support routines for the Idris 2 OCaml backend *)

exception Idris_exception of string * int;;
(*  Z.t comes from the Zarith library *)
type ival =  I of int | BI of Z.t | D of float | C of char | S of string | CON of con | NCON of ncon | TT of bool
and con = { tag : int; vals : ival array }
and ncon = { name : string; args : ival array };;

let idris_putStr str = print_string str

(* End of support library *)


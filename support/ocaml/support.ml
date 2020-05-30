(* Support routines for the Idris 2 OCaml backend *)

exception Idris_exception of string * int;;
(*  Z.t comes from the Zarith library *)
type ival =  I of int | BI of Z.t | D of float | C of char | S of string | CON of con | NCON of ncon | TT of bool
and con = { tag : int; vals : ival array }
and ncon = { name : string; args : ival array };;

let idris_putStr str = print_string str

let idris_add a b = 
  match (a,b) with
    | (I a, I b) -> I (a + b)
    | (BI a, BI b) -> BI (Z.add a b)
    | (D a, D b) -> D (a +. b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_sub a b = 
  match (a,b) with
    | (I a, I b) -> I (a - b)
    | (BI a, BI b) -> BI (Z.sub a b)
    | (D a, D b) -> D (a -. b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_mul a b = 
  match (a,b) with
    | (I a, I b) -> I (a * b)
    | (BI a, BI b) -> BI (Z.mul a b)
    | (D a, D b) -> D (a *. b)
    | _ -> raise (Invalid_argument "invalid arg types");;
  
let idris_div a b = 
  match (a,b) with
    | (I a, I b) -> I (a / b)
    | (BI a, BI b) -> BI (Z.div a b)
    | (D a, D b) -> D (a /. b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_mod a b = 
  match (a,b) with
    | (I a, I b) -> I (a mod b)
    | (BI a, BI b) -> BI (Z.rem a b)
    | (D a, D b) -> D (mod_float a b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_neg a = 
  match a with
    | (I a) -> I (-a)
    | (BI a) -> BI (Z.neg a)
    | (D a) -> D (-.a)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_shl a b = 
  match (a,b) with
    | (I a, I b) -> I (a lsl b)
    | (BI a, BI b) -> BI (Z.shift_left a (Z.to_int b))
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_shr a b = 
  match (a,b) with
    | (I a, I b) -> I (a lsr b)
    | (BI a, BI b) -> BI (Z.shift_right a (Z.to_int b))
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_and a b = 
  match (a,b) with
    | (I a, I b) -> I (a land b)
    | (BI a, BI b) -> BI (Z.logand a b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_or a b = 
  match (a,b) with
    | (I a, I b) -> I (a lor b)
    | (BI a, BI b) -> BI (Z.logor a b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_xor a b = 
  match (a,b) with
    | (I a, I b) -> I (a lxor b)
    | (BI a, BI b) -> BI (Z.logxor a b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_bool a = if a then I 1 else I 0

let idris_lt a b = 
  match (a,b) with
    | (I a, I b) -> idris_bool (a < b)
    | (BI a, BI b) -> idris_bool (Z.lt a b)
    | (D a, D b) -> idris_bool (a < b)
    | (S a, S b) -> idris_bool (a < b)
    | (C a, C b) -> idris_bool (a < b)
    | _ -> raise (Invalid_argument "invalid arg types")

let idris_lte a b = 
  match (a,b) with
    | (I a, I b) -> idris_bool (a <= b)
    | (BI a, BI b) -> idris_bool (Z.leq a b)
    | (D a, D b) -> idris_bool (a <= b)
    | (S a, S b) -> idris_bool (a <= b)
    | (C a, C b) -> idris_bool (a <= b)
    | _ -> raise (Invalid_argument "invalid arg types")

let idris_eq a b = 
  match (a,b) with
    | (I a, I b) -> idris_bool (a = b)
    | (BI a, BI b) -> idris_bool (Z.equal a b)
    | (D a, D b) -> idris_bool (a = b)
    | (S a, S b) -> idris_bool (a = b)
    | (C a, C b) -> idris_bool (a = b)
    | _ -> raise (Invalid_argument "invalid arg types")

let idris_gte a b = 
  match (a,b) with
    | (I a, I b) -> idris_bool (a >= b)
    | (BI a, BI b) -> idris_bool (Z.geq a b)
    | (D a, D b) -> idris_bool (a >= b)
    | (S a, S b) -> idris_bool (a >= b)
    | (C a, C b) -> idris_bool (a >= b)
    | _ -> raise (Invalid_argument "invalid arg types")

let idris_gt a b = 
  match (a,b) with
    | (I a, I b) -> idris_bool (a > b)
    | (BI a, BI b) -> idris_bool (Z.gt a b)
    | (D a, D b) -> idris_bool (a > b)
    | (S a, S b) -> idris_bool (a > b)
    | (C a, C b) -> idris_bool (a > b)
    | _ -> raise (Invalid_argument "invalid arg types")
(* End of support library *)


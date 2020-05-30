(* Support routines for the Idris 2 OCaml backend *)

exception Idris_exception of string;;
(*  Z.t comes from the Zarith library *)
type ival =  I of int | BI of Z.t | D of float | C of char | S of string | CON of con | NCON of ncon | TT of bool | FUN of (ival -> ival) | V
and con = { tag : int; vals : ival array }
and ncon = { name : string; args : ival array };;

let idris_putStr str = print_string str

let idris_apply f a =
  match f with
    | FUN fn -> fn a
    | _ -> raise (Idris_exception "trying to call non function")

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
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_lte a b =
  match (a,b) with
    | (I a, I b) -> idris_bool (a <= b)
    | (BI a, BI b) -> idris_bool (Z.leq a b)
    | (D a, D b) -> idris_bool (a <= b)
    | (S a, S b) -> idris_bool (a <= b)
    | (C a, C b) -> idris_bool (a <= b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_eq a b =
  match (a,b) with
    | (I a, I b) -> idris_bool (a = b)
    | (BI a, BI b) -> idris_bool (Z.equal a b)
    | (D a, D b) -> idris_bool (a = b)
    | (S a, S b) -> idris_bool (a = b)
    | (C a, C b) -> idris_bool (a = b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_gte a b =
  match (a,b) with
    | (I a, I b) -> idris_bool (a >= b)
    | (BI a, BI b) -> idris_bool (Z.geq a b)
    | (D a, D b) -> idris_bool (a >= b)
    | (S a, S b) -> idris_bool (a >= b)
    | (C a, C b) -> idris_bool (a >= b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_gt a b =
  match (a,b) with
    | (I a, I b) -> idris_bool (a > b)
    | (BI a, BI b) -> idris_bool (Z.gt a b)
    | (D a, D b) -> idris_bool (a > b)
    | (S a, S b) -> idris_bool (a > b)
    | (C a, C b) -> idris_bool (a > b)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_strlen a =
  match a with
    | S str -> I (String.length str)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_strhead a =
  match a with
    | S str -> C (String.get str 0)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_strtail a =
  match a with
    | S str -> S (Str.string_after str 1)
    | _ -> raise (Invalid_argument "invalid arg types");;


let idris_strindex a i =
  match (a, i) with
    | (S str, I i) -> C (String.get str i)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_strappend a b =
  match (a, b) with
    | (S a, S b) -> S (String.concat "" [a;b])
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_strrev a =
  match a with
    | S str -> S (String.of_seq (List.to_seq (Seq.fold_left (fun a b -> b::a) [] (String.to_seq str))))
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_substr i l s =
  match (i, l, s) with
    | (I i, I l, S s) ->
      let len = max l (String.length s - i) in
        S (String.sub s i len)
    | _ -> raise (Invalid_argument "invalid arg types");;


let idris_exp a =
  match a with
    | D x -> D (exp x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_log a =
  match a with
    | D x -> D (log x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_sin a =
  match a with
    | D x -> D (sin x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_cos a =
  match a with
    | D x -> D (cos x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_tan a =
  match a with
    | D x -> D (tan x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_asin a =
  match a with
    | D x -> D (asin x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_acos a =
  match a with
    | D x -> D (acos x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_atan a =
  match a with
    | D x -> D (atan x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_sqrt a =
  match a with
    | D x -> D (sqrt x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_floor a =
  match a with
    | D x -> D (floor x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_ceil a =
  match a with
    | D x -> D (ceil x)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_crash msg = raise (Idris_exception msg)

let idris_to_string a =
  match a with
    | I a -> S (string_of_int a)
    | BI a -> S (Z.to_string a)
    | D a -> S (string_of_float a)
    | S a -> S a
    | C a -> S (String.make 1 a)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_to_int a =
  match a with
    | I a -> I a
    | BI a-> I (Z.to_int a)
    | D a -> I (int_of_float a)
    | S a -> I (int_of_string a)
    | C a -> I (int_of_char a)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_to_integer a =
  match a with
    | I a -> BI (Z.of_int a)
    | BI a-> BI a
    | D a -> BI (Z.of_float a)
    | S a -> BI (Z.of_string a)
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_to_double a =
  match a with
    | I a -> D (float_of_int a)
    | BI a-> D (Z.to_float a)
    | D a -> D a
    | S a -> D (float_of_string a)
    | C a -> D (float_of_int (int_of_char a))
    | _ -> raise (Invalid_argument "invalid arg types");;

let idris_to_char a =
  match a with
    | I a -> C (char_of_int a)
    | _ -> raise (Invalid_argument "invalid arg types");;

























(* End of support library *)


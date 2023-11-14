(* Question 1.1 *)
type bigint = int64 list;;

val print_bigint : bigint -> unit
val bigint_to_string : bigint -> string
val pow2 : int64 -> int64

(* Question 1.2 *)
val decomposition : bigint -> bool list

(* Question 1.3 *)
val completion : bool list -> int -> bool list

(* Question 1.4 *)
val composition : bool list -> bigint

(* Question 1.5 *)
val table : bigint -> int -> bool list

(* Question 1.6 *)
val genalea : int64 -> bigint
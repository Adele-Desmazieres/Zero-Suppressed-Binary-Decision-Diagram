open Int64

(* Representation d'un bigint en liste d'int64 *)
type bigint = int64 list;;

(* Binary Decision Diagram *)
type BDD =
  | Leaf of bool
  | Node of BDD * int * BDD (* TODO: int or int64 or bigint ??? *)
;;


(* Question 1.1 *)

let peek b =
  match b with
  | [] -> raise (Invalid_argument "peek: empty b")
  | e::b2 -> e
;;

let pop b =
  match b with
  | [] -> raise (Invalid_argument "pop: empty b")
  | e::b2 -> (e, b2)
;;

let rec insert x b =
  match b with
  | [] -> [x]
  | e::b2 -> e::(insert x b2)
;;

let rec print_bigint b =
  match b with
  | [] -> print_string " "
  | e::b2 ->
      print_string (to_string e);
      print_string ", ";
      print_bigint b2
;;


(* Question 1.2 *)

let rec modulo_int64 x y =
  if compare x y = -1
  then x
  else modulo_int64 (sub x y) y
;;

let rec decomposition_int64 x =
  if x = 0L then [false]
  else if x = 1L then [true]
  else if (modulo_int64 x 2L) = 1L
  then true::(decomposition_int64 (div x 2L))
  else false::(decomposition_int64 (div x 2L))
;;

let rec decomposition b =
  match b with
  | [] -> []
  | x::b2 -> decomposition_int64 x @ decomposition b2
;;


(* Fonctions auxiliaires *)

let rec pow2 n =
  match n with
    0L -> 1L
  | _ -> mul 2L (pow2 (sub n 1L))
;;

let c2 = [pow2 2L];;
decomposition c2;;

let rec list_of_pow n =
  if n < 64L then [pow2 n]
  else 0L::(list_of_pow (sub n 64L))
;;


(* Question 1.3 *)

let rec completion l n =
  match (l,n) with
    (_,0) -> []
  | ([],n) -> false::(completion l (n-1))
  | (h::t,n) -> h::(completion t (n-1))
;;


(* Question 1.4 *)

let rec composition l =
  [let rec compo l pow =
     match l with
       [] -> 0L
     | h::t ->
         if h = false then compo t (add pow 1L)
         else add (pow2 pow) (compo t (add pow 1L))
   in compo l 0L]
;;


(* Question 1.5 *)

let table x n =
  completion (decomposition [x]) n








(* TESTS *)

let b = [23L; 6L];;
let b = insert 4L b;;
print_bigint b;;
let e = peek b;;
let (e, b) = pop b;;
print_bigint b;;

let b2 = [38L];;
decomposition b2;;
let b3 = [0L;38L];;
decomposition b3;;

list_of_pow 63L;;
list_of_pow 100L;;
list_of_pow 36L;;

completion [false; true; true; false; false; true] 4;;
completion [false; true; true; false; false; true] 8;;

composition [false; true; true; false; false; true];;


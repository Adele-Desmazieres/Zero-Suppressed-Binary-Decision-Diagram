(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

open Int64


type bigint = int64 list;;


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

let b = [23L; 6L];;
let b = insert 4L b;;
print_bigint b;;
let e = peek b;;
let (e, b) = pop b;;
print_bigint b;;


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

let b2 = [38L];;
decomposition b2;;


(* Question 1.3 *)



















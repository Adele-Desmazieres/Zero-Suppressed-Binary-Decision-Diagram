(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

open Int64


type int_list64 =
  | Empty
  | Cell of int64 * int_list64
;;

(* Question 1.1 *)

let peek l =
  match l with 
  | Empty -> raise (Invalid_argument "peek: empty l")
  | Cell(e, l2) -> e
;; 

let pop l =
  match l with 
  | Empty -> raise (Invalid_argument "pop: empty l")
  | Cell(e, l2) -> (e, l2)
;;

let rec insert x l =
  match l with
  | Empty -> Cell(x, Empty)
  | Cell(e, l2) -> Cell(e, insert x l2)
;;
  
let rec print_int_list l = 
  match l with 
  | Empty -> print_string " "
  | Cell(e, l2) -> 
      print_string (to_string e); 
      print_string ", ";
      print_int_list l2
;;

let l = Cell(23L, Cell(6L, Empty));; 
let l = insert 4L l;;
print_int_list l;;
let e = peek l;;
let (e, l) = pop l;;
print_int_list l;;


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

let rec decomposition l =
  match l with
  | Empty -> []
  | Cell(x, l2) -> decomposition_int64 x @ decomposition l2
;;

let l2 = Cell(38L, Empty);;
decomposition l2;;


(* Question 1.3 *)












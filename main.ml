open Int64

(* Representation d'un bigint en liste d'int64 *)
type bigint = int64 list;;

(* Binary Decision Diagram *)
type bdd = 
  | Leaf of bool
  | Node of bdd * int * bdd
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


(* Question 2.7 : voir le type bdd *)

let rec print_arbre a =
  match a with
  | Leaf(true) -> print_string "Leaf true" 
  | Leaf(false) -> print_string "Leaf false" 
  | Node(a1, b, a2) -> 
      print_string "Node ("; 
      print_arbre a1;
      print_string ", ";
      print_int b;
      print_string ", ";
      print_arbre a2;
      print_string ")"
;; 


(* Question 2.8 *)

let cons_arbre vertable =
  
  let rec cons_arbre_aux vertable start_index end_index current_depth =
    if start_index + 1 = end_index 
    then Leaf(List.nth vertable start_index)
    else 
      let mid_index = (start_index + end_index) / 2 in
      let a1 = cons_arbre_aux vertable start_index mid_index (current_depth+1) in
      let a2 = cons_arbre_aux vertable mid_index end_index (current_depth+1) in
      Node(a1, current_depth, a2) 
  in
  
  cons_arbre_aux vertable 0 (List.length vertable) 1
;;











(* TESTS *)
(*
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
*)
      
let a = cons_arbre [true; false; false; false];;
print_arbre a;;
print_string "\n";;


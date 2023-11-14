open Printf
open Bigint


(* Question 2.7 : Binary Decision Diagram *)
type bdd =
  | Leaf of bool
  | Node of bdd * int * bdd
;;


let print_arbre a =
  let rec print_arbre_aux a =
    match a with
    | Leaf(true) -> print_string "Leaf true"
    | Leaf(false) -> print_string "Leaf false"
    | Node(a1, b, a2) ->
        print_string "Node (";
        print_arbre_aux a1;
        print_string ", ";
        print_int b;
        print_string ", ";
        print_arbre_aux a2;
        print_string ")"
  in
  print_arbre_aux a;
  print_string "\n"
;;


(* Question 2.8 *)

(* Source : https://github.com/janestreet/base/blob/master/src/int.ml *)
(* Returns the smallest power of 2 that is greater than or equal to [x] *)
let ceil_pow2 x =
  if x <= 0 then raise (Invalid_argument "pow2");
  let x = x - 1 in
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  (* The next line is superfluous on 32-bit architectures, but it's faster to do it
     anyway than to branch *)
  let x = x lor (x lsr 32) in
  x + 1
;;


(* Input: bool list of any size.
   Output: a decision binary tree that contains the elements
   of the list in its leaves, ordered from left to right. *)
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
  let size = List.length vertable in
  (* On complète la liste avec des false pour qu'elle ai comme taille la prochaine puissance de 2 *)
  let newsize = ceil_pow2 size in
  let vertable_completed = completion vertable newsize in
  printf "size=%d, newsize=%d\n" size newsize;
  cons_arbre_aux vertable_completed 0 newsize 1
;;


(* Question 2.9 *)
(* Renvoie la liste de booléens correspondant à 
   la liste de feuilles du bdd a, de gauche à droite *)
let rec liste_feuilles a =
  match a with
  | Leaf(e) -> [e]
  | Node(a1, e, a2) -> liste_feuilles a1 @ liste_feuilles a2
;;

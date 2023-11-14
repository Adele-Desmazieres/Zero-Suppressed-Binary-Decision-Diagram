(* Question 2.7 *)
type bdd =
  | Leaf of bool
  | Node of bdd * int * bdd
;;

val print_arbre : bdd -> unit

(* Question 2.8 *)
val cons_arbre : bool list -> bdd

(* Question 2.9 *)
val liste_feuilles : bdd -> bool list
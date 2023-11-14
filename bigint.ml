open Int64
open Printf
open Random
let () = Random.self_init ();


(* Question 1.1 : Représentation d'un bigint en liste d'int64 *)
type bigint = int64 list;;


(* Renvoie le premier élément d'une liste *)
let peek b =
  match b with
  | [] -> raise (Invalid_argument "peek: empty b")
  | e::b2 -> e
;;

(* Renvoie le couple (premier élément d'une liste, le reste) *)
let pop b =
  match b with
  | [] -> raise (Invalid_argument "pop: empty b")
  | e::b2 -> (e, b2)
;;

(* Insère l'élément x à la fin de la liste b *)
let rec insert x b =
  match b with
  | [] -> [x]
  | e::b2 -> e::(insert x b2)
;;

(* Affiche un bigint *)
let print_bigint b =
  let rec print_bigint_aux b =
    match b with
    | [] -> printf "EMPTY"
    | [e] -> printf "%s]\n" (to_string e)
    | e::b2 ->
        print_string (to_string e);
        printf "; ";
        print_bigint_aux b2
  in
  printf "[";
  print_bigint_aux b
;;

let rec bigint_to_string b =
  match b with
  | [] -> "EMPTY"
  | [e] -> to_string e ^ "L"
  | e::b2 -> to_string e ^ "L; " ^ (bigint_to_string b2)
;;

(* Question 1.2 *)

(* Renvoie 2 à la puissance n *)
let rec pow2 n =
  match n with
    0L -> 1L
  | _ -> mul 2L (pow2 (sub n 1L))
;;

(* Return le résultat de x mod y (négatifs ou positifs) *)
let rec modulo_int64_basic x y =
  if ((x >= 0L && y > 0L) || (x < 0L && y < 0L)) then sub x (mul (div x y) y)
  else modulo_int64_basic (add (modulo_int64_basic x (sub 0L y)) y) y
;;

(* Return le résultat de x mod y
(En considérant que les vraies valeurs des entiers compris entre 2^63 et 2^64 - 1 sont représentées par les négatifs) *)
let rec modulo_int64 x y =
  if (x >= 0L && y > 0L) then modulo_int64_basic x y
  else if (x < 0L && y > 0L)
  then modulo_int64_basic (add (modulo_int64_basic (sub (pow2 63L) 1L) y) (modulo_int64 (sub x (sub (pow2 63L) 1L)) y)) y
  else if (x >= 0L && y < 0L) then x
  else if (x < y) then x
  else sub x y
;;

(* Return la liste des booléennes d'un int64 *)
let rec decomposition_int64 x =
  if x = 0L then [false]
  else if x = 1L then [true]
  else if (modulo_int64 x 2L) = 1L
  then
    if (x >= 0L) then true::(decomposition_int64 (div x 2L))
    else true::(decomposition_int64 (sub (add (pow2 63L) (div x 2L)) 1L))
  else if (x >= 0L) then false::(decomposition_int64 (div x 2L))
  else false::(decomposition_int64 (add (pow2 63L) (div x 2L)))
;;

(* Return la liste des booléennes d'un int64 complété à 64 éléments par des false à la fin *)
(* Cette fonction aurait pu être écrite en utilisant la fonction précédente et la fonction complétion
mais dans le sujet, la fonction complétion vient après cette question *)
let dec_64_int64 x =
  let rec decompos x n =
    match n with
      0 -> []
    | _ ->
        if (modulo_int64 x 2L) = 1L
        then
          if (x >= 0L) then true::(decompos (div x 2L) (n-1))
          else true::(decompos (sub (add (pow2 63L) (div x 2L)) 1L) (n-1))
        else if (x >= 0L) then false::(decompos (div x 2L) (n-1))
        else false::(decompos (add (pow2 63L) (div x 2L)) (n-1))
  in
  decompos x 64
;;

(* Return la liste des booléennes d'un bigint *)
let rec decomposition b =
  match b with
  | [] -> []
  | [x] -> decomposition_int64 x
  | x::y::b2 -> dec_64_int64 x @ decomposition (y::b2)
;;


(* Question 1.3 *)

(* Return une liste de taille n contenant les éléments de l et des false à la fin : (taille de l) < n *)
(* Return une liste de taille n contenant les n premiers éléments de l : (taille de l) >= n *)
let rec completion l n =
  match (l,n) with
    (_,0) -> []
  | ([],n) -> false::(completion l (n-1))
  | (h::t,n) -> h::(completion t (n-1))
;;


(* Question 1.4 *)

(* Return une liste de liste, chacune des sous-listes contient n éléments de la liste *)
(* paquets [1;2;3;4;5;6;7;8;9;10;11] 3 returns [ [1;2;3] ; [4;5;6] ; [7;8;9] ; [10;11] ] *)
let paquets liste n =
  (* par convention, on renvoie une liste vide si n = 0 *)
  if n = 0 then []
  else
    (* on va parcourir la liste et la scinder aux bons endroits *)
    let rec aux liste k = match liste with
      | [] -> [[]]
      | elt :: queue ->
          (* si k est arrivé à 0, on repart de n *)
          if k = 0 then [] :: aux liste n
          else match (aux queue (k-1)) with
            | [] -> failwith "ce cas ne devrait pas arriver"
            | paquet_courant :: reste -> (elt :: paquet_courant) :: reste
    in aux liste n
;;

(* Return un int64 correspondant à la liste de valeurs booléennes *)
let rec composit l =
  let rec compo l pow =
    match l with
      [] -> 0L
    | h::t ->
        if h = false then compo t (add pow 1L)
        else add (pow2 pow) (compo t (add pow 1L))
  in compo l 0L
;;

(* La fonction prend une liste de liste de booléennes et renvoie le bigint correspondant
en appliquant la fonction précédente sur chacune de ces sous-listes *)
let rec comp ll =
  match ll with
    [] -> []
  | h::t -> (composit h)::(comp t)
;;

(*  bool list -> int64 list (un bigint) *)
let rec composition l =
  match l with
    [] -> [0L]
  | h::t -> comp (paquets l 64)
;;


(* Question 1.5 *)

(* Générer la table de vérité d'un bigint *)
let table x n =
  completion (decomposition x) n
;;


(* Question 1.6 *)

(* shift_n_left décalle l'entier x de n bits à gauche *)
let shift_n_left x n =
  mul x (pow2 n)
;;

(* Génère un bigint représentant un entier encodé sur n bits, autrement dit un entier entre 0 et 2^n exclu *)
let rec genalea n =
  if n <= 64L then let v = add (Random.int64 (pow2 (div n 2L))) (shift_n_left (Random.int64 (pow2 (sub n (div n 2L)))) (div n 2L))
    in if v = 0L then [] else [v]
  else (genalea 64L) @ (genalea (sub n 64L))
;;


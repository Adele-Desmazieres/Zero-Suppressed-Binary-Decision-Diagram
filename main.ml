open Int64
open Printf

(* Question 1.1 : représentation d'un bigint en liste d'int64 *)
type bigint = int64 list;;

(* Question 2.7 : Binary Decision Diagram *)
type bdd = 
  | Leaf of bool
  | Node of (bdd ref) * int * (bdd ref)
;; 

(* Question 3.10 : liste de couples (bigint * pointeur vers noeud d'un bdd) *)
(* TODO : remplacer bdd par une ref vers un noeud *)
type listeDejaVus = (bigint * bdd) list;;



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

let rec composit l =
  let rec compo l pow =
    match l with
      [] -> 0L
    | h::t ->
        if h = false then compo t (add pow 1L)
        else add (pow2 pow) (compo t (add pow 1L))
  in compo l 0L
;;

let rec comp ll =
  match ll with
    [] -> []
  | h::t -> (composit h)::(comp t)
;;

let rec composition l =
  match l with
    [] -> [0L]
  | h::t -> comp (paquets l 64)
;;

(* Question 1.5 *)

let table x n =
  completion (decomposition [x]) n


(* Question 2.7 : voir le type bdd *)

let print_arbre a =
  let rec print_arbre_aux a =
    match a with
    | Leaf(true) -> print_string "Leaf true" 
    | Leaf(false) -> print_string "Leaf false" 
    | Node(a1, b, a2) -> 
        print_string "Node ("; 
        print_arbre_aux !a1;
        print_string ", ";
        print_int b;
        print_string ", ";
        print_arbre_aux !a2;
        print_string ")"
  in
  print_arbre_aux a;
  print_string "\n"
;; 


(* Question 2.8 *)

(* 
  Input: bool list of size of any power of 2.
  Output: a decision binary tree that contains the elements 
  of the list in its leaves, ordered from left to right. 
*)
let cons_arbre vertable =
  
  let rec cons_arbre_aux vertable start_index end_index current_depth =
    if start_index + 1 = end_index 
    then Leaf(List.nth vertable start_index)
    else 
      let mid_index = (start_index + end_index) / 2 in
      let a1 = cons_arbre_aux vertable start_index mid_index (current_depth+1) in
      let a2 = cons_arbre_aux vertable mid_index end_index (current_depth+1) in
      Node(ref a1, current_depth, ref a2) 
  in
  
  cons_arbre_aux vertable 0 (List.length vertable) 1
;;


(* Question 2.9 *)

let rec liste_feuilles a =
  match a with
  | Leaf(e) -> [e]
  | Node(a1, e, a2) -> liste_feuilles (!a1) @ liste_feuilles (!a2)
;;


(* Question 3.11 *)

(* TODO: supprimer *)
let rec parcours_suffixe a =
  match a with
  | Leaf(e) -> if e = true then print_string "true " else print_string "false "
  | Node(a1, e, a2) -> 
      parcours_suffixe !a1;
      parcours_suffixe !a2;
      printf "%d " e
;;

(* bigint_list -> listeDejaVus -> option ref bdd *)
let rec get_second_componant b ldv =
  match ldv with
  | [] -> None
  | (lb, lpointeur)::ldv2 -> 
      if lb = b
      then Some lpointeur
      else get_second_componant b ldv2
;;

(* IDEM mais == au lieu de = *)
(* TODO : regrouper cette fonction avec la précédente avec un argument 
  en plus pour indiquer test d'égalité structurelle (=) ou d'égalité physique (==) *)
(* TODO : renommer les variables dans le code et génériser les types (en 'a) dans commentaires *)
let rec get_second_componant_ref target_first_comp pairs_list =
  match pairs_list with
  | [] -> None
  | (first_comp, second_comp)::pairs_list2 -> 
      if first_comp == target_first_comp
      then Some second_comp
      else get_second_componant_ref target_first_comp pairs_list2
;;

(*
(* TODO : supprimer OU à réécrire avec des ref plutot que des noeuds *)
(* bigint_list 
   -> bdd 
   -> (bigint_list * ref bdd) listDejaVus 
   -> (bigint_list * ref bdd) listDejaVus *)
let rec overwrite_seconde_composante b new_pointeur ldv =
  match ldv with
  | [] -> raise (Invalid_argument "overwrite_seconde_composante: bigint not found")
  | (lb, lpointeur)::ldv2 -> 
      if lb = b
      then (lb, new_pointeur)::ldv2
      else (lb, lpointeur)::(overwrite_seconde_composante b new_pointeur ldv2)
;;
*)

(* TODO : compléter avec la regle suivante "Si la deuxième moitié de la 
liste ne contient que des valeurs false alors remplacer le pointeur vers N 
(depuis son parent) vers un pointeur vers l’enfant gauche de N" *)
(* bdd -> listDejaVus -> bdd *)
let rec compressionParListeAux a ldv =
  match a with
  | Leaf(e) -> (Leaf(e), ldv)
  | Node(a1, e, a2) -> 
      let (a1bis, ldv) = compressionParListeAux (!a1) ldv in
      let (a2bis, ldv) = compressionParListeAux (!a2) ldv in
      
      let n = composition (liste_feuilles (Node(a1, e, a2))) in
      let seconde_comp = (get_second_componant n ldv) in
      let new_node = Node(ref a1bis, e, ref a2bis) in
      let ldv2 =
        match seconde_comp with
        | None -> (n, new_node)::ldv
        | Some pointeur -> ldv
      in
      (new_node, ldv2)
;;


(* TODO : regrouper la partie Leaf et la partie Node sous une fonction qui prend
  tout en argument sauf gauche et droite, et renvoie tout ce qui est modifié. 
  Appeler cette fonction une fois depuis Leaf et une fois depuis Node. *)
(* bdd current_node
  -> string father_name
  -> bool is_gauche
  -> list (ref bdd * string) nodes_names_visited
  -> int id
  -> string
Fonctionnement : 
  Parcours suffixe de l'arbre. 
  A chaque noeud visité, s'il n'est pas déjà dans la liste des noeuds visités, 
  alors on lui crée un nom unique et on le concatène au string. Dans tous les cas,
  on concatène ensuite le string correspondant au lien entre son père et lui. 
  Et on appelle récursivement sur son fils gauche puis droit. 
*)
let rec toStringDotFormatAux 
    current_node 
    father_name 
    is_gauche 
    nodes_names_visited 
    id =
  
  let getNameStrListId e_str current_node father_name is_gauche nodes_names_visited id =
    let current_string = "" in
    let line_style = (if is_gauche then "[style=dashed]" else "") in
    let name_option = get_second_componant_ref current_node nodes_names_visited in
    let (name, nodes_names_visited, current_string, id) =
      match name_option with
      | None -> 
          let n = Printf.sprintf "node_%d" id in 
          let l = (current_node, n)::nodes_names_visited in
          let s = current_string ^ Printf.sprintf "\t%s [label=%s]\n" n e_str in
          let i = id + 1 in
          (n, l, s, i)
      | Some name_option -> 
          (name_option, nodes_names_visited, current_string, id)
    in 
    let current_string = (
      if father_name = "root" then current_string else
      current_string ^ Printf.sprintf "\t%s -> %s %s\n" father_name name line_style
    ) in
    (name, current_string, nodes_names_visited, id)
  in 
    
  match current_node with
  | Leaf(e) -> 
      let (name, current_string, nodes_names_visited, id) = 
        getNameStrListId (Bool.to_string e) current_node father_name is_gauche nodes_names_visited id
      in (current_string, nodes_names_visited, id)
      
  | Node(gauche, e, droite) -> 
      let (name, current_string, nodes_names_visited, id) = 
        getNameStrListId (string_of_int e) current_node father_name is_gauche nodes_names_visited id 
      in
      let (gauche_string, nodes_names_visited, id) = 
        toStringDotFormatAux (!gauche) name true nodes_names_visited id 
      in
      let (droite_string, nodes_names_visited, id) = 
        toStringDotFormatAux (!droite) name false nodes_names_visited id 
      in
      let final_string = current_string ^ gauche_string ^ droite_string in
      (final_string, nodes_names_visited, id)
;;


(* Renvoie le string correspondant à la description de l'arbre au format dot *)
let toStringDotFormat current_node =
  let (node_str, _, _) = toStringDotFormatAux current_node "root" false [] 1 in
  "digraph Q {\n" ^ node_str ^ "}\n"
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

let c2 = [pow2 2L];;
decomposition c2;;

list_of_pow 63L;;
list_of_pow 100L;;
list_of_pow 36L;;

completion [false; true; true; false; false; true] 4;;
completion [false; true; true; false; false; true] 8;;

composition [false; true; true; false; false; true];;
*)



let a = cons_arbre [true; true; true; false];;
print_arbre a;;
let la = liste_feuilles a;;
List.iter (printf "%b ") la;;
print_string "\n";;
parcours_suffixe a;;
print_string "\n";;
print_string "\n";;


let (a2, ldv2) = compressionParListeAux a [];;
print_arbre a2;;
List.iter (fun (a, b) -> print_bigint a) ldv2;;
print_string "\n";;

(*
let a_str = toStringDotFormat a;;
print_string a_str;;

let a2_str = toStringDotFormat a2;;
print_string a2_str;;

let n2 = Leaf(true);;
let n1 = Node(ref n2, 1, ref n2);;
print_string (toStringDotFormat n1);;
*)

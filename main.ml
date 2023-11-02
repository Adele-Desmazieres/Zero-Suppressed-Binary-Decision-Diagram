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
(* TODO : remplacer bdd par une ref vers un noeud ??? *)
type listeDejaVus = (bigint * bdd) list;;

(* Question 4.15 : Arbre de noeuds deja vus *)
type arbreDejaVus = 
  | Empty
  | NodeRef of arbreDejaVus * (bdd option) * arbreDejaVus


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

let rec modulo_int64_basic x y =
  if ((x >= 0L && y > 0L) || (x < 0L && y < 0L)) then sub x (mul (div x y) y)
  else modulo_int64_basic (add (modulo_int64_basic x (sub 0L y)) y) y
;;

let rec modulo_int64 x y =
  if (x >= 0L && y > 0L) then modulo_int64_basic x y
  else if (x < 0L && y > 0L)
  then modulo_int64_basic (add (modulo_int64_basic (sub (pow2 63L) 1L) y) (modulo_int64 (sub x (sub (pow2 63L) 1L)) y)) y
  else if (x >= 0L && y < 0L) then x
  else if (x < y) then x
  else sub x y
;;

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


let rec decomposition b =
  match b with
  | [] -> []
  | [x] -> decomposition_int64 x
  | x::y::b2 -> dec_64_int64 x @ decomposition (y::b2)
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
  completion (decomposition x) n
;;

(* Question 1.6 *)

(* shift_n_left décalle l'entier x de n bits à gauche *)
let shift_n_left x n =
  mul x (pow2 n)
;;

(* genalea génère un entier encodé sur n bits, autrement dit un entier entre 0 et 2^n exclu *)
let rec genalea n =
  if n <= 64L then let v = add (Random.int64 (pow2 (div n 2L))) (shift_n_left (Random.int64 (pow2 (sub n (div n 2L)))) (div n 2L))
    in if v = 0L then [] else [v]
  else (genalea 64L) @ (genalea (sub n 64L))
;;

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

(* Renvoie la deuxième composante du couple de la liste qui respecte cette condition :
    "1ère_comp_recherchée operator 1ère_comp_de_la_liste" est vraie *)
(* bigint_list -> listeDejaVus -> option ref bdd *)
let rec get_second_componant target_first_comp operator pairs_list =
  match pairs_list with
  | [] -> None
  | (first_comp, second_comp)::pairs_list2 ->
      if operator target_first_comp first_comp
      then Some second_comp
      else get_second_componant target_first_comp operator pairs_list2
;;

(* Si la val "n" du sous-arbre "current_node" est dans ldv,
  renvoie une ref vers la seconde composante du couple, et ldv inchangée.
  Sinon renvoie une ref vers le noeud, et ldv à laquelle (n, ref current_node) a été ajouté. *)
let treatNodeCompression current_node n ldv =
  let seconde_comp = (get_second_componant n (=) ldv) in
  match seconde_comp with
  | None -> (ref current_node, (n, ref current_node)::ldv)
  | Some pointeur -> (pointeur, ldv)
;;

(* Renvoie le nouvel arbre compressé et
  la liste des couples (bigint, bdd) des noeuds visités. 
  Omission de la règle Z, car les noeuds ayant des false dans la 2nde moitié de leur liste
  seront toujours merged à d'autres noeuds par la règle M. *)
(* bdd -> listDejaVus -> (bdd, listDejaVus) *)
let rec compressionParListeAux current_node ldv =
  let (new_node, ldv) =
    match current_node with
    | Leaf(e) -> (current_node, ldv)
    | Node(g, e, d) ->
    
        (* Récupérer liste de feuille avant modification *)
        let g_feuilles_composees = composition (liste_feuilles !g) in
        let d_feuilles_composees = composition (liste_feuilles !d) in
        
        (* Faire de la récurrence en suivant le parcours suffixe *)
        let (g1, ldv) = compressionParListeAux (!g) ldv in
        let (d1, ldv) = compressionParListeAux (!d) ldv in
        
        (* Récupérer le pointeur de noeud de meme val ou ajout dans la liste des noeuds déjà visités *)
        let (g1, ldv) = treatNodeCompression (!g1) g_feuilles_composees ldv in
        let (d1, ldv) = treatNodeCompression (!d1) d_feuilles_composees ldv in
        
        (* Renvoyer le nouveau noeud *)
        let new_node = Node(g1, e, d1) in
        (new_node, ldv)
  in
  (ref new_node, ldv)
;;


(* bdd -> bdd *)
let compressionParListe arbre =
  let (new_arbre, ldv) = compressionParListeAux arbre [] in 
  new_arbre
;;


(* Question 3.12 *)

(* bdd current_node
  -> string father_name
  -> bool is_gauche
  -> list (ref bdd * string) nodes_names_visited
  -> int id
  -> string
Fonctionnement :
  Parcours préfixe de l'arbre.
  A chaque noeud visité, s'il n'est pas déjà dans la liste des noeuds visités,
  alors on lui crée un nom unique qu'on concatène au string renvoyé. Dans tous les cas,
  on concatène ensuite le string correspondant au lien entre son père et lui.
  On appelle récursivement sur son fils gauche puis son fils droit.
*)
let rec toStringDotFormatAux
    current_node
    is_gauche
    father_name
    nodes_names_visited
    id =

  let getNameStrListId e_str current_node is_gauche father_name nodes_names_visited id =
    let current_string = "" in
    let line_style = (if is_gauche then "[style=dashed]" else "") in
    let name_option = get_second_componant current_node (==) nodes_names_visited in
    let (name, nodes_names_visited, current_string, id, already_visited) =
      match name_option with
      | None ->
          let n = Printf.sprintf "node_%d" id in
          let l = (current_node, n)::nodes_names_visited in
          let s0 = (
            if e_str = "true"
            then Printf.sprintf "\t%s [label=%s,  style=\"filled\", fillcolor=\"green\"]\n" n e_str
            else if e_str = "false"
            then Printf.sprintf "\t%s [label=%s,  style=\"filled\", fillcolor=\"red\"]\n" n e_str
            else Printf.sprintf "\t%s [label=%s]\n" n e_str
            )
          in
          let s = current_string ^ s0 in
          let i = id + 1 in
          let v = false in
          (n, l, s, i, v)
      | Some name_option ->
          (name_option, nodes_names_visited, current_string, id, true)
    in
    let current_string = (
      if father_name = "root" then current_string else
      current_string ^ Printf.sprintf "\t%s -> %s %s\n" father_name name line_style
    ) in
    (name, current_string, nodes_names_visited, id, already_visited)
  in

  match current_node with
  | Leaf(e) ->
      let (name, current_string, nodes_names_visited, id, already_visited) =
        getNameStrListId (Bool.to_string e) current_node is_gauche father_name nodes_names_visited id
      in (current_string, nodes_names_visited, id)

  | Node(gauche, e, droite) ->
      let (name, current_string, nodes_names_visited, id, already_visited) =
        getNameStrListId (string_of_int e) current_node is_gauche father_name nodes_names_visited id in

      let (gauche_string, nodes_names_visited, id) = (
        if not already_visited
        then toStringDotFormatAux (!gauche) true name nodes_names_visited id
        else ("", nodes_names_visited, id)) in

      let (droite_string, nodes_names_visited, id) = (
        if not already_visited
        then toStringDotFormatAux (!droite) false name nodes_names_visited id
        else ("", nodes_names_visited, id)) in

      let final_string = current_string ^ gauche_string ^ droite_string in
      (final_string, nodes_names_visited, id)
;;


(* Renvoie le string correspondant à la description de l'arbre au format dot *)
let toStringDotFormat current_node =
  let (node_str, _, _) = toStringDotFormatAux current_node false "root" [] 1 in
  "digraph Q {\n" ^ node_str ^ "}\n"
;;


(* TODO : modifier cette fonction pour décharger le contenu ligne par ligne ou dans un buffer *)
let exportDot filename content =
  let oc = open_out filename in (* create or truncate file, return channel *)
  Printf.fprintf oc "%s" content; (* write something *)
  close_out oc (* close channel *)
;;




(* TESTS PARTIES 1 ET 2 *)
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


(* TESTS PARTIE 3 *)

let n50 = Leaf(false);;
let n51 = Leaf(true);;
let n41 = Node(ref n50, 4, ref n51);;
let n42 = Node(ref n51, 4, ref n51);;
let n31 = Node(ref n42, 3, ref n41);;
let n32 = Node(ref n41, 3, ref n51);;
let n33 = Node(ref n51, 3, ref n51);;
let n21 = Node(ref n31, 2, ref n41);;
let n22 = Node(ref n33, 2, ref n32);;
let n1 = Node(ref n21, 1, ref n22);;

(* let n3 = n2;; *)
(* let n4 = n2;; *)
(* Printf.printf "fils égaux ? %b\n" (n3 == n4);; *)
(* Printf.printf "ref fils égales ? %b\n" (ref n3 = ref n4);; *)
(* let n1 = Node(ref n3, 1, ref n4);; *)
(* print_string (toStringDotFormat n1);; *)

let print_ldv ldv =
  print_string "\nldv3 = ";
  List.iter (fun (a, b) ->
    print_string "(";
    print_bigint a;
    print_string "";
    let () = match (!b) with
      | Leaf(e) -> print_string (string_of_bool e)
      | Node(a1, e, a2) -> print_int e in
    print_string "), " )
  ldv
;;

(* let a3 = cons_arbre [true; true; true; false];; *)
let a3 = cons_arbre [true; true; false; true; false; true; false; false; true; false; true; false; false; true; true; false];;
let a3_str = toStringDotFormat a3;;
(* print_string a3_str;; *)
let (a3c, ldv3) = compressionParListeAux a3 [];;
(* print_ldv ldv3;; *)
(* print_string "\n\n";; *)
let a3c_str = toStringDotFormat (!a3c);;
(* print_string a3c_str;; *)

exportDot "BDD_25899.dot" a3_str;;
exportDot "BDD_25899_compressed.dot" a3c_str;;



print_string "Done.\n"
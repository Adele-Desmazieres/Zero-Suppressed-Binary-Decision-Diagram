open Printf

open Decisiondiagram
open Bigint


(* Question 3.10 : Liste de couples (bigint * pointeur vers noeud d'un bdd) *)
type listeDejaVus = (bigint * bdd) list;;

(* Question 4.15 : Arbre de noeuds deja vus *)
type arbreDejaVus =
  | Empty
  | NodeADV of arbreDejaVus * bdd option * arbreDejaVus
;;


(* Question 3.11 *)

(* Renvoie la deuxième composante du couple de la liste qui respecte cette condition :
    "1ère_comp_recherchée operator 1ère_comp_de_la_liste" est vraie *)
(* bigint -> listeDejaVus -> option bdd *)
let rec get_second_componant target_first_comp operator pairs_list =
  match pairs_list with
  | [] -> None
  | (first_comp, second_comp)::pairs_list2 ->
      if operator target_first_comp first_comp
      then Some second_comp
      else get_second_componant target_first_comp operator pairs_list2
;;

(* Si la val "n" du sous-arbre "current_node" est dans ldv,
  renvoie la seconde composante du couple, et ldv inchangée.
  Sinon renvoie le noeud, et ldv à laquelle (n, current_node) a été ajouté. *)
let treatNodeCompression current_node n ldv =
  let seconde_comp = (get_second_componant n (=) ldv) in
  match seconde_comp with
  | None -> (current_node, (n, current_node)::ldv)
  | Some pointeur -> (pointeur, ldv)
;;

(* Renvoie le nouvel arbre compressé et
  la liste des couples (bigint, bdd) des noeuds visités.
  Omission de la règle Z, car les noeuds ayant des false dans la 2nde moitié de leur liste
  seront toujours merged à d'autres noeuds par la règle M. *)
(* bdd -> listDejaVus -> (bdd, listDejaVus) *)
let rec compressionParListeAux current_node ldv =
  match current_node with
  | Leaf(e) -> (current_node, ldv)
  | Node(g, e, d) ->
      
      (* Récupérer liste de feuille avant modification *)
      let g_feuilles_composees = composition (liste_feuilles g) in
      let d_feuilles_composees = composition (liste_feuilles d) in

      (* Règle Z *)
      if d_feuilles_composees = [0L]
      then
        let (g1, ldv) = compressionParListeAux g ldv in
        let (g1, ldv) = treatNodeCompression g1 g_feuilles_composees ldv in
        (g1, ldv)
      else

      (* Faire de la récurrence en suivant le parcours suffixe *)
      let (g1, ldv) = compressionParListeAux g ldv in
      let (d1, ldv) = compressionParListeAux d ldv in

      (* Récupérer le pointeur de noeud de meme val ou ajout dans la liste des noeuds déjà visités *)
      let (g1, ldv) = treatNodeCompression g1 g_feuilles_composees ldv in
      let (d1, ldv) = treatNodeCompression d1 d_feuilles_composees ldv in

      (* Renvoyer le nouveau noeud *)
      let new_node = Node(g1, e, d1) in
      (new_node, ldv)
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
  -> list (bdd * string) nodes_names_visited
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
        then toStringDotFormatAux gauche true name nodes_names_visited id
        else ("", nodes_names_visited, id)) in

      let (droite_string, nodes_names_visited, id) = (
        if not already_visited
        then toStringDotFormatAux droite false name nodes_names_visited id
        else ("", nodes_names_visited, id)) in

      let final_string = current_string ^ gauche_string ^ droite_string in
      (final_string, nodes_names_visited, id)
;;


(* Renvoie le string correspondant à la description de l'arbre au format dot *)
let toStringDotFormat current_node =
  let (node_str, _, _) = toStringDotFormatAux current_node false "root" [] 1 in
  "digraph Q {\n" ^ node_str ^ "}\n"
;;

(* Exporte le string content dans un fichier nommé filename.
   L'écrase s'il existe déjà. *)
let exportDot filename content =
  let oc = open_out filename in (* create or truncate file, return channel *)
  Printf.fprintf oc "%s" content; (* write something *)
  close_out oc (* close channel *)
;;


(* Question 4.16 et 4.17 *)

(* Renvoie l'élément recherché et adv éventuellement modifié *)
let rec insertOrGetADV adv nodeBDD vertable =
  match vertable with
  | [] -> (
      match adv with
      | Empty -> (nodeBDD, NodeADV (Empty, Some nodeBDD, Empty))
      | NodeADV (g1, e1, d1) as n -> (
          match e1 with
          | None -> (nodeBDD, NodeADV (g1, Some nodeBDD, d1))
          | Some new_node -> (new_node, n)
        )
    )
  | b::v2 ->
      match adv with
      | Empty ->
          if b
          then let (new_node, new_d1) = insertOrGetADV Empty nodeBDD v2 in
            (new_node, NodeADV (Empty, None, new_d1))
          else let (new_node, new_g1) = insertOrGetADV Empty nodeBDD v2 in
            (new_node, NodeADV (new_g1, None, Empty))
      | NodeADV (g1, e1, d1) ->
          if b
          then let (new_node, new_d1) = insertOrGetADV d1 nodeBDD v2 in
            (new_node, NodeADV (g1, e1, new_d1))
          else let (new_node, new_g1) = insertOrGetADV g1 nodeBDD v2 in
            (new_node, NodeADV (new_g1, e1, d1))
;;


(* Renvoie le nouvel arbre compressé et
  l'arbreDejaVus des noeuds visités. *)
(* bdd -> arbreDejaVus -> (bdd, arbreDejaVus) *)
let rec compressionParArbreAux current_node adv =
  match current_node with
  | Leaf(e) -> (current_node, adv)
  | Node(g, e, d) ->

      (* Récupérer liste de feuille avant modification *)
      let g_feuilles = liste_feuilles g in
      let d_feuilles = liste_feuilles d in

      (* Règle Z *)
      if composition d_feuilles = [0L]
      then
        let (g1, adv) = compressionParArbreAux g adv in
        let (g1, adv) = insertOrGetADV adv g1 g_feuilles in
        (g1, adv)

      else
        (* Faire de la récurrence gauche *)
        let (g1, adv) = compressionParArbreAux g adv in
        let (d1, adv) = compressionParArbreAux d adv in

        (* Récupérer le pointeur de noeud de meme val ou ajouter dans la liste des noeuds déjà visités *)
        let (g1, adv) = insertOrGetADV adv g1 g_feuilles in
        let (d1, adv) = insertOrGetADV adv d1 d_feuilles in

        (* Renvoyer le nouveau noeud *)
        let new_node = Node(g1, e, d1) in
        (new_node, adv)
;;


(* bdd -> bdd *)
let compressionParArbre arbre =
  let (new_arbre, adv) = compressionParArbreAux arbre Empty in
  new_arbre
;;


(* Exécute la compression par liste ou par arbre sur le bigint b 
   et exporte les fichiers dot des arbres avant et après compression. *)
let bigintToDot filename b isParListe =
  let vertable = decomposition b in
  let arbre = cons_arbre vertable in
  let arbre_compressed =
    if isParListe
    then compressionParListe arbre
    else compressionParArbre arbre
  in
  let title = filename in
  exportDot (title^".dot") (toStringDotFormat arbre);
  exportDot (title^"_compressed.dot") (toStringDotFormat arbre_compressed);
  ()
;;


let rec has l elt =
  match l with
  | [] -> false
  | e::l2 -> if e == elt then true else has l2 elt
;;


let count_nodes arbre =
  
  let rec count_nodes_aux arbre nodes_liste =  
    
    match arbre with
    | Leaf(e) -> 
      if has nodes_liste arbre
      then nodes_liste, 0
      else arbre::nodes_liste, 1
        
    | Node(g, e, d) -> 
      let nodes_liste_g, n_g = count_nodes_aux g nodes_liste in
      let nodes_liste_gd, n_d = count_nodes_aux d nodes_liste_g in
      
      if has nodes_liste arbre
      then nodes_liste, 0
      else arbre::nodes_liste_gd, (n_g + n_d + 1)
  in
  let _,res = count_nodes_aux arbre [] in res
;;


open Int64
open Printf
open Random
let () = Random.self_init ();

(* Question 1.1 : Représentation d'un bigint en liste d'int64 *)
type bigint = int64 list;;

(* Question 2.7 : Binary Decision Diagram *)
type bdd =
  | Leaf of bool
  | Node of bdd * int * bdd
;;

(* Question 3.10 : Liste de couples (bigint * pointeur vers noeud d'un bdd) *)
type listeDejaVus = (bigint * bdd) list;;

(* Question 4.15 : Arbre de noeuds deja vus *)
type arbreDejaVus =
  | Empty
  | NodeADV of arbreDejaVus * bdd option * arbreDejaVus
;;



(* Fonctions auxiliaires *)
(* Return 2 à la puissance n *)
let rec pow2 n =
  match n with
    0L -> 1L
  | _ -> mul 2L (pow2 (sub n 1L))
;;

(* Return le bigint qui correspond à 2 à la puissance n *)
let rec list_of_pow n =
  if n < 64L then [pow2 n]
  else 0L::(list_of_pow (sub n 64L))
;;

(* Question 1.1 *)

(* Return le premier élément d'une liste *)
let peek b =
  match b with
  | [] -> raise (Invalid_argument "peek: empty b")
  | e::b2 -> e
;;

(* Return le couple (premier élément d'une liste , le reste) *)
let pop b =
  match b with
  | [] -> raise (Invalid_argument "pop: empty b")
  | e::b2 -> (e, b2)
;;

(* Insérer l'élément x à la fin de la liste b *)
let rec insert x b =
  match b with
  | [] -> [x]
  | e::b2 -> e::(insert x b2)
;;

(* Afficher un bigint *)
let print_bigint b =
  let rec print_bigint_aux b =
    match b with
    | [] -> print_string "EMPTY"
    | [e] -> printf "%s]\n" (to_string e)
    | e::b2 ->
        print_string (to_string e);
        print_string "; ";
        print_bigint_aux b2
  in
  printf "[";
  print_bigint_aux b
;;


(* Question 1.2 *)

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


(*
  Input: bool list of any size.
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

let rec liste_feuilles a =
  match a with
  | Leaf(e) -> [e]
  | Node(a1, e, a2) -> liste_feuilles a1 @ liste_feuilles a2
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



(* TESTS DU PROJET *)

(*
let tests () = 
  
    ()
;;
*)

let print_bool_list l =
  let rec pblaux l =
    match l with 
    | [] -> ()
    | [e] -> printf "%b" e
    | e::l2 -> printf "%b, " e; pblaux l2
  in
  printf "[";
  pblaux l;
  printf "]\n"
;;


(* TESTS PARTIES 1 ET 2 *)
let tests_1_2 () =
  let b = [23L; 6L] in
  let b = insert 4L b in
  print_bigint b;
  let e = peek b in
  printf "%Lu" e;
  let (e, b) = pop b in
  print_bigint b;
    
  let b2 = [38L] in 
  print_bool_list (decomposition b2);
  
  let b3 = [0L;38L] in 
  print_bool_list (decomposition b3);
  
  let c2 = [pow2 2L] in 
  print_bool_list (decomposition c2);
  
  print_bigint (list_of_pow 63L);
  print_bigint (list_of_pow 100L);
  print_bigint (list_of_pow 36L);
  
  print_bool_list (completion [false; true; true; false; false; true] 4);
  print_bool_list (completion [false; true; true; false; false; true] 8);
  
  print_bigint (composition [false; true; true; false; false; true])
;;


(* TESTS PARTIE 3 *)
let tests_3 () =
  let n50 = Leaf(false) in
  let n51 = Leaf(true) in
  let n41 = Node(n50, 4, n51) in
  let n42 = Node(n51, 4, n51) in
  let n31 = Node(n42, 3, n41) in
  let n32 = Node(n41, 3, n51) in
  let n33 = Node(n51, 3, n51) in
  let n21 = Node(n31, 2, n41) in
  let n22 = Node(n33, 2, n32) in
  let n1 = Node(n21, 1, n22) in
  
  print_string (toStringDotFormat n1);
  
  let vertable = decomposition [25899L] in
  print_bool_list vertable;
  
  let a4 = cons_arbre vertable in
  let a4c = compressionParListe a4 in
  print_string (toStringDotFormat a4);
  print_string (toStringDotFormat a4c);
  printf "\n";
  
  let b = [-4077520903251697734L; 3725098519587869504L] in (* = 68715939040191755864311453423018172346 *)
  print_bigint b;
  bigintToDot "130_bits_LDV" b true
;;

  
(* TESTS PARTIE 4 *)
let tests_4 () = 
  
  let dirname = "./tests_4/" in
  if not (Sys.file_exists dirname) then Sys.mkdir dirname 0o755;
  
  let standard_test_4 b filename =
    print_bigint b;
    bigintToDot (dirname ^ filename ^ "_LDV") b true;
    bigintToDot (dirname ^ filename ^ "_ADV") b false;
    printf "\n"
  in
  
  standard_test_4 [25899L] "25899L";
  standard_test_4 [7L] "7L";
  standard_test_4 [0L; 1L] "0L_1L";
  standard_test_4 [9223372036854775808L; 1L] "2pow63_1L";
  standard_test_4 (genalea 1000L) "e3_bits";
  standard_test_4 (genalea 2000L) "2e3_bits";
  standard_test_4 (genalea 3000L) "3e3_bits";
  (* standard_test_4 (genalea 10000L) "e4_bits"; *)
  
  (* Compressions différentes entre par arbre et par liste à partir d'environ 3000 bits.
     Par exemple, le bigint suivant (sur 2999 bits) donne deux compressions différentes :

  [-7397863826597483745; -7770261156321738754; 1236646854865449075; -5190928585146045899;
  -4992808975050723075; -5741163498365487965; 7946033078052947069; -2800735776790153635;
  -3918936533814437455; -8998001415999952448; -6294537310539991044; -7668792850197517654;
  8563250432377705112; -552875915331482884; -6668039009565253450; -881992044850089135;
  -5531170330478518348; -3552442076188947852; 5565918363508105768; -9161973353537052590;
  -5502829082253087124; -3043477099894200854; 4834271741461236247; 3176104119376014797;
  8173290540801592168; -4800686476214529944; 2343392459435193004; -7177155205930299855;
  8863579622411367543; 7547174846486908704; 7196820598621166457; 1761421479576635729;
  -6805764409314456464; -8737641954624191063; -3284744692263401815; 1879820879237403955;
  -1170213225930205699; -5764086671883100520; -6879513556585695580; -4857462984785588336;
  6258772459160726337; -1049649545095147934; -8986565433282590063; 3399141512103088421;
  -1858961850604665463; 7974041216154821691; 31402785079399147]
  *)
;;

let main () =
  (* tests_1_2 (); *)
  (* tests_3 (); *)
  tests_4 ()
;;

main ();;
print_string "Done.\n";;

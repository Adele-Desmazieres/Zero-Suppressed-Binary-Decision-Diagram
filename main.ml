open Int64
open Printf

open Bigint
open Decisiondiagram
open Compression


(* Fonctions auxiliaires de tests *)

(* Renvoie le bigint qui correspond à 2 à la puissance n *)
let rec list_of_pow n =
  if n < 64L then [pow2 n]
  else 0L::(list_of_pow (sub n 64L))
;;

(* Affiche une liste de bool *)
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
  (* if not (Sys.file_exists dirname) then Sys.mkdir dirname 0o755; *)
  
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
  standard_test_4 (genalea 10000L) "e4_bits";
  
  let b = [-7397863826597483745L; -7770261156321738754L; 1236646854865449075L; -5190928585146045899L;
  -4992808975050723075L; -5741163498365487965L; 7946033078052947069L; -2800735776790153635L;
  -3918936533814437455L; -8998001415999952448L; -6294537310539991044L; -7668792850197517654L;
  8563250432377705112L; -552875915331482884L; -6668039009565253450L; -881992044850089135L;
  -5531170330478518348L; -3552442076188947852L; 5565918363508105768L; -9161973353537052590L;
  -5502829082253087124L; -3043477099894200854L; 4834271741461236247L; 3176104119376014797L;
  8173290540801592168L; -4800686476214529944L; 2343392459435193004L; -7177155205930299855L;
  8863579622411367543L; 7547174846486908704L; 7196820598621166457L; 1761421479576635729L;
  -6805764409314456464L; -8737641954624191063L; -3284744692263401815L; 1879820879237403955L;
  -1170213225930205699L; -5764086671883100520L; -6879513556585695580L; -4857462984785588336L;
  6258772459160726337L; -1049649545095147934L; -8986565433282590063L; 3399141512103088421L;
  -1858961850604665463L; 7974041216154821691L; 31402785079399147L] in
  standard_test_4 b "test2";

  
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
  
  [-7397863826597483745L; -7770261156321738754L; 1236646854865449075L; -5190928585146045899L;
  -4992808975050723075L; -5741163498365487965L; 7946033078052947069L; -2800735776790153635L;
  -3918936533814437455L; -8998001415999952448L; -6294537310539991044L; -7668792850197517654L;
  8563250432377705112L; -552875915331482884L; -6668039009565253450L; -881992044850089135L;
  -5531170330478518348L; -3552442076188947852L; 5565918363508105768L; -9161973353537052590L;
  -5502829082253087124L; -3043477099894200854L; 4834271741461236247L; 3176104119376014797L;
  8173290540801592168L; -4800686476214529944L; 2343392459435193004L; -7177155205930299855L;
  8863579622411367543L; 7547174846486908704L; 7196820598621166457L; 1761421479576635729L;
  -6805764409314456464L; -8737641954624191063L; -3284744692263401815L; 1879820879237403955L;
  -1170213225930205699L; -5764086671883100520L; -6879513556585695580L; -4857462984785588336L;
  6258772459160726337L; -1049649545095147934L; -8986565433282590063L; 3399141512103088421L;
  -1858961850604665463L; 7974041216154821691L; 31402785079399147L]

  *)
;;


let execution_times filename n_bits_init n_bits_max n_bits_step =
  let oc = open_out filename in (* create or truncate file, return channel *)
  Printf.fprintf oc "nb_bits_genalea,temps_liste,temps_arbre\n";
  
  let rec forloop n_bits =
    
    let b = genalea n_bits in
    let vertable = decomposition b in
    let arbre = cons_arbre vertable in
    
    (* comparaison par liste *)
    let t_init = Sys.time() in (* initialisation du temps *)
    let _ = compressionParListe arbre in
    let t_liste = Sys.time() -. t_init in
    
    (* comparaison par arbre *)
    let t_init = Sys.time() in (* initialisation du temps *)
    let _ = compressionParArbre arbre in
    let t_arbre = Sys.time() -. t_init in
    
    (* export temps en csv *)
    Printf.fprintf oc "%Lu,%f,%f\n" n_bits t_liste t_arbre;
    
    if (add n_bits n_bits_step < n_bits_max)
    then forloop (add n_bits n_bits_step)
    else ()
    
  in
  
  forloop n_bits_init;
  close_out oc (* close channel *)
;;


let compression_rates filename n_bits_init n_bits_max n_bits_step =
  let oc = open_out filename in (* create or truncate file, return channel *)
  Printf.fprintf oc "bigint,nb_bits_genalea,nb_noeuds_depart,nb_noeuds_liste,nb_noeuds_arbre\n";
  
  let rec forloop n_bits =
    
    let b = genalea n_bits in
    let vertable = decomposition b in
    let arbre = cons_arbre vertable in
    let nb_noeuds = count_nodes arbre in
    
    (* comparaison par liste *)
    let a_ldv = compressionParListe arbre in
    let nb_noeuds_liste = count_nodes a_ldv in
    
    (* comparaison par arbre *)
    let a_adv = compressionParArbre arbre in
    let nb_noeuds_arbre = count_nodes a_adv in
    
    (* export temps en csv *)
    Printf.fprintf oc "%Lu,%d,%d,%d\n" n_bits nb_noeuds nb_noeuds_liste nb_noeuds_arbre;
    
    if (add n_bits n_bits_step < n_bits_max)
    then forloop (add n_bits n_bits_step)
    else ()
    
  in
  
  forloop n_bits_init;
  close_out oc (* close channel *)
;;


(* TESTS PARTIE 6 *)
let tests_6 () =
  (* execution_times "execution_times.csv" 10L 3001L 20L; *)
  (*
  let b = [25899L] in
  let vertable = decomposition b in
  let arbre = cons_arbre vertable in
  let a_ldv = compressionParListe arbre in
  
  let avant_compression = count_nodes arbre in
  let apres_compression = count_nodes a_ldv in
  
  printf "avant_compression = %d\n" avant_compression;
  printf "apres_compression = %d\n" apres_compression;
  
  let b = [-91584709871724310L; -2401588754722799053L; 3L] in
  bigintToDot "debug_liste" b true; 
  bigintToDot "debug_arbre" b false; 
  *)
  compression_rates "compression_rates.csv" 10L 3001L 20L; 
;;


let main () =
  (* tests_1_2 (); *)
  (* tests_3 (); *)
  (* tests_4 (); *)
  tests_6 ();
;;

main ();;
print_string "Done.\n";;

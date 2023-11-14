open Bigint
open Decisiondiagram

(* Question 3.10 *)
type listeDejaVus = (Bigint.bigint * Decisiondiagram.bdd) list;;

(* Question 4.15 *)
type arbreDejaVus =
  | Empty
  | NodeADV of arbreDejaVus * Decisiondiagram.bdd option * arbreDejaVus
;;

val compressionParListe : Decisiondiagram.bdd -> Decisiondiagram.bdd

val toStringDotFormat : Decisiondiagram.bdd -> string

val exportDot : string -> string -> unit

val compressionParArbre : Decisiondiagram.bdd -> Decisiondiagram.bdd

val bigintToDot : string -> Bigint.bigint -> bool -> unit
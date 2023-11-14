open Bigint
open Decisiondiagram


val compressionParListe : Decisiondiagram.bdd -> Decisiondiagram.bdd

val toStringDotFormat : Decisiondiagram.bdd -> string

val exportDot : string -> string -> unit

val compressionParArbre : Decisiondiagram.bdd -> Decisiondiagram.bdd

val bigintToDot : string -> Bigint.bigint -> bool -> unit

val count_nodes : Decisiondiagram.bdd -> int
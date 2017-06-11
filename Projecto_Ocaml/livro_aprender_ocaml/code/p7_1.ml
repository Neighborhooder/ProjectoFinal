(* Graphes non orientés par matrices d'adjacence

   Même structure que pour un graphe orienté, avec l'invariant que
   pour chaque arc a->b on a également l'arc b->a *)

type vertex = int
type t = bool array array

let create n = Array.make_matrix n n false
let nb_vertex = Array.length

let mem_edge g v1 v2 = g.(v1).(v2)

(* changements ici *)
let add_edge g v1 v2 =
  g.(v1).(v2) <- true; g.(v2).(v1) <- true
let remove_edge g v1 v2 =
  g.(v1).(v2) <- false; g.(v2).(v1) <- false

let iter_succ f g v =
  Array.iteri (fun w b -> if b then f w) g.(v)

(* changement ici aussi, pour ne parcourir chaque arc qu'une seule fois *)
let iter_edge f g =
  for v = 0 to nb_vertex g - 1 do
    for w = v to nb_vertex g - 1 do (* seulement au dessus de la diagonale *)
      if g.(v).(w) then f v w
    done
  done

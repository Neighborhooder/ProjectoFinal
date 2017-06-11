(***********************************************************************)
(*                                                                     *)
(*  OCaml library from the book ``Apprendre à programmer avec OCaml''  *)
(*                                                                     *)
(*  Sylvain Conchon and Jean-Christophe Filliâtre                      *)
(*  Université Paris Sud                                               *)
(*                                                                     *)
(*  Copyright 2014 Université Paris Sud.  All rights reserved. This    *)
(*  file is distributed under the terms of the GNU Library General     *)
(*  Public License, with the same special exception on linking as the  *)
(*  OCaml library. See http://caml.inria.fr/ocaml/license.fr.html      *)
(*                                                                     *)
(***********************************************************************)

(* Programme 68 page 281
   Graphe par matrice d'adjacence *)

  type vertex = int
  type t = bool array array

  let create n = Array.make_matrix n n false
  let nb_vertex = Array.length

  let mem_edge g v1 v2 = g.(v1).(v2)
  let add_edge g v1 v2 = g.(v1).(v2) <- true
  let remove_edge g v1 v2 = g.(v1).(v2) <- false

  let iter_succ f g v = Array.iteri (fun w b -> if b then f w) g.(v)
  let iter_edge f g =
    for v = 0 to nb_vertex g - 1 do iter_succ (f v) g v done

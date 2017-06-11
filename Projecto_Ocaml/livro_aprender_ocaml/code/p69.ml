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

(* Programme 69 page 284
   Graphe par listes d'adjacence *)

  type vertex = int
  type t = vertex list array

  let create n = Array.make  n []
  let nb_vertex = Array.length

  let mem_edge g v1 v2 = 
    List.mem v2 g.(v1)

  let add_edge g v1 v2 =
    if not (mem_edge g v1 v2) then g.(v1) <- v2 :: g.(v1)

  let remove_edge g v1 v2 = 
    g.(v1) <- List.filter ((<>) v2) g.(v1)

  let iter_succ f g v = 
    List.iter f g.(v)

  let iter_edge f g =
    for v = 0 to nb_vertex g - 1 do iter_succ (f v) g v done

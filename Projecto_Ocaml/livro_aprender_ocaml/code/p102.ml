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

(* Programme 102 page 406
   Algorithme de Bellman-Ford *)

  let iter_edge f g =
    iter_vertex (fun u ->
      iter_succ (fun v -> f u v (weight g u v)) g u) g

  exception NegativeCycle

  let bellman_ford g s =
    let h = H.create () in
    iter_vertex (fun v -> H.add h v max_float) g;
    H.add h s 0.;
    for i = 1 to nb_vertex g - 1 do
      iter_edge (fun u v w ->
        let d = H.find h u +. w in
        if d < H.find h v then H.replace h v d
      ) g
    done;
    iter_edge (fun u v w ->
      if H.find h u +. w < H.find h v then
        raise NegativeCycle
    ) g;
    h

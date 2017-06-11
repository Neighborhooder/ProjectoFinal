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

(* Programme 99 page 396
   Parcours en largeur *)

  let iter_bfs f g s =
    let visited = H.create () in
    let queue = Queue.create () in
    let add w = H.add visited w (); Queue.add w queue in
    add s;
    while not (Queue.is_empty queue) do
      let v = Queue.pop queue in
      f v;
      iter_succ (fun w -> if not (H.mem visited w) then add w) g v
    done

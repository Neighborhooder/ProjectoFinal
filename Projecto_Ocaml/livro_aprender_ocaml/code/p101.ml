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

(* Programme 101 page 403
   Plus court chemin (algorithme de Dijkstra) *)

  module VertexDistance = struct
    type t = vertex * float
    let compare (_, d1) (_, d2) = Pervasives.compare d1 d2
  end
  module P = PriorityQueue(VertexDistance)

  let rec dijkstra f g s =
    let visited = H.create () in
    let distance = H.create () in
    let queue = P.create () in
    let add v d = H.replace distance v d; P.add queue (v, d) in
    add s 0.;
    while not (P.is_empty queue) do
      let (u, du) = P.extract_min queue in
      if not (H.mem visited u) then begin
        H.add visited u ();
        f u du;
        let visit v =
          let d = du +. weight g u v in
          if not (H.mem distance v) || d < H.find distance v then
            add v d
        in
        iter_succ visit g u
      end
    done

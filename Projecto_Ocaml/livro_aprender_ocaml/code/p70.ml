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

(* Programme 70 page 288
   Graphes par dictionnaire d'adjacence *)

module Graph(H: HashTable) = struct
  module V = H
  type vertex = V.key

  module E = H

  type t = (unit E.t) V.t

  let create () = V.create ()
  let nb_vertex g = V.length g

  let mem_vertex g v = V.mem g v
  let add_vertex g v = V.add g v (E.create ())
  let remove_vertex g v =
    V.remove g v;
    V.iter (fun _ s -> E.remove s v) g

  let mem_edge g v1 v2 = E.mem (V.find g v1) v2
  let add_edge g v1 v2 = E.replace (V.find g v1) v2 ()
  let remove_edge g v1 v2 = E.remove (V.find g v1) v2

  let iter_vertex f g = V.iter (fun v _ -> f v) g
  let iter_succ f g v = E.iter (fun w _ -> f w) (V.find g v)
  let iter_edge f g = V.iter (fun v s -> E.iter (f v) s) g
end

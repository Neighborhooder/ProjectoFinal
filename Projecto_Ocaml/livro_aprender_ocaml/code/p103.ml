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

(* Programme 103 page 410
   Algorithme de Kruskal *)

  let spanning_tree g =
    let uf = UF.create (vertices g) in
    let compare (_,_,w1) (_,_,w2) = Pervasives.compare w1 w2 in
    let edges = List.sort compare (edges g) in
    let st = ref [] in
    let cover ((u, v, w) as e) =
      if UF.find uf u <> UF.find uf v then begin
	UF.union uf u v;
	st := e :: !st
      end
    in
    List.iter cover edges;
    !st

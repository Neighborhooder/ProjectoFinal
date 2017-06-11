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

(* Programme 51 page 231
   Intersection d'arbres de préfixes *)

  let rec inter t1 t2 =
    { word = t1.word && t2.word;
      branches = inter_branches t1.branches t2.branches; }
  and inter_branches m1 m2 =
    M.fold
      (fun i ti m ->
	 try
	   let t = inter ti (M.find i m2) in
	   if is_empty t then m else M.add i t m
	 with Not_found -> m)
      m1 M.empty

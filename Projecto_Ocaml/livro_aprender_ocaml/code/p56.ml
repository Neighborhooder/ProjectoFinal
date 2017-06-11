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

(* Programme 56 page 244
   Union de deux arbres de Patricia *)

let rec union t1 t2 = match t1, t2 with
  | Empty, t | t, Empty  ->
      t
  | Leaf x, t | t, Leaf x ->
      add x t
  | Node (p1, b1, l1, r1), Node (p2, b2, l2, r2) ->
      if b1 == b2 && matches_prefix p2 p1 b1 then
	Node (p1, b1, union l1 l2, union r1 r2)
      else if b1 < b2 && matches_prefix p2 p1 b1 then
	if zero_bit p2 b1 then
	  Node (p1, b1, union l1 t2, r1)
        else
	  Node (p1, b1, l1, union r1 t2)
      else if b1 > b2 && matches_prefix p1 p2 b2 then
	if zero_bit p1 b2 then
	  Node (p2, b2, union t1 l2, r2)
	else
	  Node (p2, b2, l2, union t1 r2)
      else
	branch p1 t1 p2 t2

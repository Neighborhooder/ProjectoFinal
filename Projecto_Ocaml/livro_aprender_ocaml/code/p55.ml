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

(* Programme 55 page 241
   Suppression dans un arbre de Patricia *)

let node = function
  | (_, _, Empty, t)
  | (_, _, t, Empty) -> t
  | (p, b, l, r) -> Node (p, b, l, r)

let rec remove x = function
  | Empty ->
      Empty
  | Leaf j as t ->
      if x == j then Empty else t
  | Node (p, m, t0, t1) as t ->
      if matches_prefix x p m then
	if zero_bit x m then
	  node (p, m, remove x t0, t1)
	else
	  node (p, m, t0, remove x t1)
      else
	t

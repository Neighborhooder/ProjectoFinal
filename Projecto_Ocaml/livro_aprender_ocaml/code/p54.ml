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

(* Programme 54 page 238
   Insertion dans un arbre de Patricia *)

let rightmost_1_bit x =
  x land -x

let branch p1 t1 p2 t2 =
  let b = rightmost_1_bit (p1 lxor p2) in
  let p = p1 land (b-1) in
  if zero_bit p1 b then
    Node (p, b, t1, t2)
  else
    Node (p, b, t2, t1)

let matches_prefix x p b =
  x land (b-1) == p

let rec add x = function
  | Empty ->
      Leaf x
  | Leaf j as t ->
      if j == x then t else branch x (Leaf x) j t
  | Node (p, b, l, r) as t ->
      if matches_prefix x p b then
	if zero_bit x b then
	  Node (p, b, add x l, r)
	else
	  Node (p, b, l, add x r)
      else
	branch x (Leaf x) p t

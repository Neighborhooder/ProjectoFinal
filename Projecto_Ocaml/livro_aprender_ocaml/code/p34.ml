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

(* Programme 34 page 201
   Arbres binaires de recherche (1/2) *)

  type t = Empty | Node of t * elt * t

  let rec min_elt = function
    | Empty -> raise Not_found
    | Node (Empty, v, __) -> v
    | Node (l, _, _) -> min_elt l

  let rec mem x = function
    | Empty ->
	false
    | Node (l, v, r) ->
	let c = X.compare x v in
	c = 0 || if c < 0 then mem x l else mem x r

  let rec add x t =
    match t with
    | Empty ->
	Node (Empty, x, Empty)
    | Node (l, v, r) ->
        let c = X.compare x v in
	if c = 0 then t
	else if c < 0 then Node (add x l, v, r)
	else Node (l, v, add x r)

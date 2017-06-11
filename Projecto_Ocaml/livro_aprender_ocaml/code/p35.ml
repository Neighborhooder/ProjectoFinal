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

(* Programme 35 page 203
   Arbres binaires de recherche (2/2) *)

  let rec remove_min_elt = function
    | Empty -> Empty
    | Node (Empty, _, r) -> r
    | Node (l, v, r) -> Node (remove_min_elt l, v, r)

  let merge t1 t2 = match t1, t2 with
    | Empty, t | t, Empty -> t
    | _ -> Node (t1, min_elt t2, remove_min_elt t2)

  let rec remove x = function
    | Empty ->
	Empty
    | Node (l, v, r) ->
	let c = X.compare x v in
	if c = 0 then merge l r
	else if c < 0 then Node (remove x l, v, r)
	else Node (l, v, remove x r)

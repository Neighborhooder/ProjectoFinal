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

(* Programme 66 page 271
   Files de priorité persistantes *)

module Make(X: Ordered) :
  PersistentPriorityQueue with type elt = X.t =
struct
  type elt = X.t
  type t = Empty | Node of t * elt * t

  let empty =
    Empty

  let is_empty h =
    h = Empty

  let get_min = function
    | Empty -> invalid_arg "get_min"
    | Node (_, x, _) -> x

  let rec merge ha hb = match ha, hb with
    | Empty, h | h, Empty ->
	h
    | Node (la, xa, ra), Node (lb, xb, rb) ->
	if X.compare xa xb <= 0 then
	  Node (ra, xa, merge la hb)
	else
	  Node (rb, xb, merge lb ha)

  let add x h =
    merge (Node (Empty, x, Empty)) h

  let remove_min = function
    | Empty -> invalid_arg "remove_min"
    | Node (a, _, b) -> merge a b
end

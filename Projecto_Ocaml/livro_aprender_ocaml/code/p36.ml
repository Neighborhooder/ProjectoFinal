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

(* Programme 36 page 209
   Équilibrage d'un AVL *)

  let balance l v r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 1 then begin
      match l with
	| Node (ll, lv, lr, _) when height ll >= height lr ->
	    node ll lv (node lr v r)
	| Node (ll, lv, Node (lrl, lrv, lrr, _),_)->
            node (node ll lv lrl) lrv (node lrr v r)
	| _ ->
	    assert false
    end else if hr > hl + 1 then begin
      match r with
	| Node (rl, rv, rr, _) when height rr >= height rl ->
	    node (node l v rl) rv rr
	| Node (Node(rll, rlv, rlr, _), rv, rr, _) ->
            node (node l v rll) rlv (node rlr rv rr)
	| _ ->
	    assert false
    end else
      node l v r

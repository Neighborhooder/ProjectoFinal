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

(* Programme 49 page 227
   Structure d'arbre de préfixes *)

module Make(L : Letter) : PersistentSet with type elt = L.t list =
struct

  module M = Map.Make(L)

  type elt = L.t list
  type t = { word : bool ; branches : t M.t; }

  let empty = { word = false; branches = M.empty }

  let is_empty t = not t.word && M.is_empty t.branches

  let rec mem x t =
    match x with
      |	[] ->
	  t.word
      | i::l ->
	  try mem l (M.find i t.branches) 
	  with Not_found -> false

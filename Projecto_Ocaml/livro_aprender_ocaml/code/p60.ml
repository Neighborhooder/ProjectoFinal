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

(* Programme 60 page 258
   Files persistantes représentées par des paires de listes *)

  type 'a t = 'a list * 'a list

  let empty =
    ([], [])

  let is_empty = function
    | [], [] -> true
    | _ -> false

  let push x (o, i) =
    (o, x :: i)

  let pop = function
    | [], [] ->
	invalid_arg "pop"
    | x :: o, i ->
	x, (o, i)
    | [], i ->
	match List.rev i with
	| x :: o -> x, (o, [])
	| [] -> assert false

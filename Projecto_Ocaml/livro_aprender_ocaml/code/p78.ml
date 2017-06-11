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

(* Programme 78 page 319
   Curseur pour les arbres (parcours infixe) *)

  type 'a enum = Top | Left of 'a * 'a tree * 'a enum

  let rec leftmost t e = match t with
    | E -> e
    | N (l, x, r) -> leftmost l (Left (x, r, e))

  let start t =
    leftmost t Top

  let step = function
    | Top -> raise Exit
    | Left (x, r, e) -> x, leftmost r e

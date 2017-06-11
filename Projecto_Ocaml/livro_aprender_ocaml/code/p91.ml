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

(* Programme 91 page 363
   Tri rapide (d'une liste) *)

  let rec partition ((left, right) as acc) p = function
    | [] -> acc
    | x :: s when le x p ->
        partition (x :: left, right) p s
    | x :: s -> 
        partition (left, x :: right) p s

  let rec quicksort = function
    | [] ->
        []
    | p :: s ->
        let (left, right) = partition ([], []) p s in
        (quicksort left) @ (p :: quicksort right)
  
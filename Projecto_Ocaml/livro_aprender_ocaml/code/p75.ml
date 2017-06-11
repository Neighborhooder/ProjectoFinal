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

(* Programme 75 page 317
   Comparaison lexicographique d'arbres binaires *)

  let rec leftmost z = function
    | E -> z
    | N (l, x, r) -> leftmost (Left (z, x, r)) l

  let rec compare cmp z1 z2 = match z1, z2 with
    | Top, Top ->
        0
    | Left (z1, x1, r1), Left (z2, x2, r2) ->
        let c = cmp x1 x2 in
        if c <> 0 then c
        else compare cmp (leftmost z1 r1) (leftmost z2 r2)
    | Top, Left _ ->
        -1
    | Left _, Top ->
        1
    | Right _, _ | _, Right _ ->
        assert false

  let compare_tree cmp t1 t2 =
    compare cmp (leftmost Top t1) (leftmost Top t2)

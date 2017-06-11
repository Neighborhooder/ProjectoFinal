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

(* Programme 93 page 370
   Tri fusion (d'une liste) *)

  let rec split l1 l2 = function
    | [] -> (l1, l2)
    | x :: l -> split (x :: l2) l1 l

  let rec merge acc l1 l2 = match l1, l2 with
    | [], l | l, [] ->
        List.rev_append acc l
    | x1 :: s1, x2 :: s2 ->
        if le x1 x2 then merge (x1 :: acc) s1 l2
        else merge (x2 :: acc) l1 s2

  let rec mergesort l = match l with
    | [] | [_] ->
        l
    | _ ->
        let l1, l2 = split [] [] l in
        merge [] (mergesort l1) (mergesort l2)
  
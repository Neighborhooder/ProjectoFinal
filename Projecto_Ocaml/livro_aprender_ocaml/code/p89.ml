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

(* Programme 89 page 359
   Tri par insertion (d'une liste) *)

  let rec insert acc x = function
    | y :: l when lt y x -> insert (y :: acc) x l
    | l -> List.rev_append acc (x :: l)

  let insertion_sort l =
    List.fold_left (fun r x -> insert [] x r) [] l
  
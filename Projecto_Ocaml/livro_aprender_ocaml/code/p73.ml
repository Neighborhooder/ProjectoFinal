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

(* Programme 73 page 308
   Structure de zipper pour une liste *)

  type 'a zipper = { left: 'a list; right: 'a list; }

  let of_list l =
    { left = []; right = l }

  let move_right z = match z.right with
    | [] -> invalid_arg "move_right"
    | x :: r -> { left = x :: z.left; right = r }

  let move_left z = match z.left with
    | [] -> invalid_arg "move_left"
    | x :: l -> { left = l; right = x :: z.right }

  let to_list z =
    List.rev_append z.left z.right

  let insert z x =
    { z with left = x :: z.left }

  let delete_left z =  match z.left with
    | [] -> invalid_arg "delete_left"
    | _ :: l -> { z with left = l }

  let delete_right z =  match z.right with
    | [] -> invalid_arg "delete_right"
    | _ :: r -> { z with right = r }

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

(* Programme 29 page 179
   Opérations de modification sur les cordes *)

  let set t i c =
    let n = length t in
    if i < 0 || i >= n then invalid_arg "set";
    sub t 0 i ++ make 1 c ++ sub t (i + 1) (n - i - 1)

  let insert t i r =
    let n = length t in
    if i < 0 || i > n then invalid_arg "insert";
    sub t 0 i ++ r ++ sub t i (n - i)

  let insert_char t i c =
    insert t i (make 1 c)

  let delete_char t i =
    let n = length t in
    if i < 0 || i >= n then invalid_arg "delete_char";
    sub t 0 i ++ sub t (i + 1) (n - i - 1)

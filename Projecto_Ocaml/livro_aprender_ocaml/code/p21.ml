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

(* Programme 21 page 163
   Lecture et écriture dans un tableau de bits *)

let get v n =
  let i = n / bpi and j = n mod bpi in
  (v.bits.(i) lsr j) land 1 <> 0

let set v n b =
  let i = n / bpi and j = n mod bpi in
  if b then
    v.bits.(i) <- v.bits.(i) lor (1 lsl j)
  else
    v.bits.(i) <- v.bits.(i) land lnot (1 lsl j)

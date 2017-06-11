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

(* Programme 20 page 161
   Création d'un tableau de bits *)

let create n b =
  let initv = if b then -1 else 0 in
  let q = n / bpi and r = n mod bpi in
  if r = 0 then
    { length = n; bits = Array.make  q initv }
  else begin
    let a = Array.make  (q + 1) initv in
    if b then a.(q) <- (1 lsl r) - 1;
    { length = n; bits = a }
  end

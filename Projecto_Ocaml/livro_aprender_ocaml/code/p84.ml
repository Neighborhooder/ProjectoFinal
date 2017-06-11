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

(* Programme 84 page 342
   Calcul de $F_n$ par mémoïsation *)

let memo = Hashtbl.create 17
let rec fib_memo n =
  try
    Hashtbl.find memo n
  with Not_found ->
    let fn =
      if n <= 1 then n else fib_memo (n-2) + fib_memo (n-1) in
    Hashtbl.add memo n fn;
    fn

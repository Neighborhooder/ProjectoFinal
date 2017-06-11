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

(* Programme 85 page 343
   Calcul de $F_n$ par programmation dynamique *)

let fib_dp n =
  if n = 0 then 0 else
  let f = Array.make (n+1) 0 in
  f.(1) <- 1;
  for i = 2 to n do f.(i) <- f.(i-2) + f.(i-1) done;
  f.(n)

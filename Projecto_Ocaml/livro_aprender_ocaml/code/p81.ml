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

(* Programme 81 page 329
   Calcul modulo $m$ (addition et soustraction) *)

  let () = assert (0 < m && m <= max_int/2 + 1)

  let of_int x = let r = x mod m in if r < 0 then r + m else r

  let add x y = let r = x + y in if r >= m then r - m else r

  let sub x y = let r = x - y in if r < 0 then r + m else r

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

(* Programme 45 page 221
   Redimensionnement d'une table de hachage *)

  let resize h =
    let n = Array.length h.buckets in
    let m = 2 * n in
    let a = Array.make m [] in
    let rehash x =
      let i = (X.hash x) mod m in
      a.(i) <- x :: a.(i)
    in
    Array.iter (List.iter rehash) h.buckets;
    h.buckets <- a

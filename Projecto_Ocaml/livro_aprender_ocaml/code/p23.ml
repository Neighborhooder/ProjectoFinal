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

(* Programme 23 page 166
   Parcours des bits à 1 d'un tableau de bits *)

let iteri_true f v =
  Array.iteri
    (fun i ei ->
       let index = i * bpi in
       let rec visit x =
	 if x <> 0 then begin
	   let b = x land -x in
	   f (index + ntz b);
	   visit (x - b)
	 end
       in
       visit ei)
    v.bits

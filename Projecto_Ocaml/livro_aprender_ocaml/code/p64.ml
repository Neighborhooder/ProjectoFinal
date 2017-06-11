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

(* Programme 64 page 267
   Structure de tas impérative (2/2) *)

  let min h l r =
    if X.compare (A.get h r) (A.get h l) < 0 then r else l

  let smallest_node h x i =
    let l = 2 * i + 1 in
    let n = A.length h in
    if l >= n then i else
      let r = l + 1 in
      let j = if r < n then min h l r else l in
      if X.compare (A.get h j) x < 0 then j else i

  let rec move_down h x i =
    let j = smallest_node h x i in
    if j = i then A.set h i x
    else begin A.set h i (A.get h j); move_down h x j end

  let remove_min h =
    let n = A.length h - 1 in
    if n < 0 then invalid_arg "remove_min";
    let x = A.get h n in
    A.resize h n;
    if n > 0 then move_down h x 0
end

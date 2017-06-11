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

(* Programme 92 page 366
   Tri rapide (d'un tableau) *)

  let swap a i j =
    let t = a.(i) in 
    a.(i) <- a.(j); 
    a.(j) <- t

  let partition a l r =
    let p = a.(l) in
    let m = ref l in
    for i = l + 1 to r - 1 do
      if le a.(i) p then begin incr m; swap a i !m end
    done;
    if l <> !m then swap a l !m;
    !m

  let rec quick_rec a l r =
    if l < r - 1 then begin
      let m = partition a l r in
      quick_rec a l m;
      quick_rec a (m + 1) r
    end

  let quicksort a = quick_rec a 0 (Array.length a)
  
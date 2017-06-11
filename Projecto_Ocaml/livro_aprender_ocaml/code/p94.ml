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

(* Programme 94 page 373
   Tri fusion (d'un tableau) *)

  let merge a1 a2 l m r =
    let i = ref l in
    let j = ref m in
    for k = l to r - 1 do
      if !i < m && (!j = r || le a1.(!i) a1.(!j)) then begin
        a2.(k) <- a1.(!i); incr i
      end else begin
        a2.(k) <- a1.(!j); incr j
      end
    done

  let mergesort a =
    let tmp = Array.copy a in
    let rec mergesort_rec l r =
      if l < r - 1 then begin
        let m = (l + r) / 2 in
        mergesort_rec l m;
        mergesort_rec m r;
        Array.blit a l tmp l (r - l);
        merge tmp a l m r
      end
    in
    mergesort_rec 0 (Array.length a)
  
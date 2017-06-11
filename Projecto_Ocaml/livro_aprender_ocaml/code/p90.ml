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

(* Programme 90 page 360
   Tri par insertion (d'un tableau) *)

  let insertion_sort a =
    for i = 1 to Array.length a - 1 do
      let v = a.(i) in
      let j = ref i in
      while 0 < !j && lt v a.(!j - 1) do
        a.(!j) <- a.(!j - 1);
        decr j
      done;
      a.(!j) <- v
    done
  
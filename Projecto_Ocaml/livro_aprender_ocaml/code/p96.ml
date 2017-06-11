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

(* Programme 96 page 376
   Tri par tas (d'une liste) *)

  let heapsort l =
    let h = Heap.create () in
    List.iter (fun x -> Heap.add x h) l;
    let res = ref [] in
    while not (Heap.is_empty h) do
      let x = Heap.get_min h in
      Heap.remove_min h;
      res := x :: !res
    done;
    !res
  
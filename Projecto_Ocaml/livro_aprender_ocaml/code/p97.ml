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

(* Programme 97 page 380
   Tri par tas (d'un tableau) *)

  let rec move_down a k v n =
    let r = 2 * k + 1 in
    if r >= n then
      a.(k) <- v
    else
      let rmax =
        if r+1 < n then if lt a.(r) a.(r+1) then r+1 else r
        else r in
      if le a.(rmax) v then a.(k) <- v
      else begin a.(k) <- a.(rmax); move_down a rmax v n end

  let heapsort a =
    let n = Array.length a in
    for k = n/2 - 1 downto 0 do move_down a k a.(k) n done;
    for k = n-1 downto 1 do
      let v = a.(k) in a.(k) <- a.(0); move_down a 0 v k
    done
  
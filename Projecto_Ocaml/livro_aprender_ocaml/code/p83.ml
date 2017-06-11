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

(* Programme 83 page 333
   Calcul matriciel élémentaire *)

type matrix = int array array

let init_matrix n m f =
  Array.init n (fun i -> Array.init m (fun j -> f i j))

let id n =
  init_matrix n n (fun i j -> if i = j then 1 else 0)

let size a =
  (Array.length a, Array.length a.(0))

let add a b =
  let (n, m) as s = size a in
  if size b <> s then invalid_arg "add";
  init_matrix n m (fun i j -> a.(i).(j) + b.(i).(j))

let mul a b =
  let n, p = size a in
  let q, m = size b in
  if q <> p then invalid_arg "mul";
  let product i j =
    let s = ref 0 in
    for k = 0 to p - 1 do s := !s + a.(i).(k) * b.(k).(j) done;
    !s
  in
  init_matrix n m product

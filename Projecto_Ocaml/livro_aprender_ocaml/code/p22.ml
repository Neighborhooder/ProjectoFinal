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

(* Programme 22 page 164
   Opérations et et non sur les tableaux de bits *)

let inter v1 v2 =
  let l1 = v1.length in
  if l1 <> v2.length then invalid_arg "Bitv.inter";
  let b = Array.mapi (fun i ei -> ei land v2.bits.(i)) v1.bits in
  { length = l1; bits = b }

let normalize v =
  let r = v.length mod bpi in
  if r > 0 then
    let s = Array.length v.bits - 1 in
    v.bits.(s) <- v.bits.(s) land (1 lsl r - 1)

let compl v =
  let b = Array.map lnot v.bits in
  let r = { length = v.length; bits = b } in
  normalize r;
  r

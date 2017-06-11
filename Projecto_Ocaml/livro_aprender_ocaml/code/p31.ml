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

(* Programme 31 page 188
   Tableaux persistants *)

type 'a t = 'a data ref
and 'a data =
  | Arr of 'a array 
  | Diff of int * 'a * 'a t
      
let init n f = ref (Arr (Array.init n f))

let rec reroot pa = match !pa with
  | Arr a -> 
      a
  | Diff (i, v, pa') -> 
      let a = reroot pa' in
      let old = a.(i) in
      a.(i) <- v;
      pa := Arr a;
      pa' := Diff (i, old, pa);
      a

let length pa = Array.length (reroot pa)

let get pa i = (reroot pa).(i)
      
let iteri f pa = Array.iteri f (reroot pa)

let set pa i v = 
  let a = reroot pa in
  let old = a.(i) in
  a.(i) <- v;
  let res = ref (Arr a) in
  pa := Diff (i, old, res);
  res

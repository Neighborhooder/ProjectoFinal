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

(* Programme 18 page 156
   Tableaux redimensionnables *)

  type 'a t = {
    default: 'a;
    mutable size: int;
    mutable data: 'a array;
  }

  let length a = a.size

  let make n d = { default = d; size = n; data = Array.make n d }

  let get a i =
    if i < 0 || i >= a.size then invalid_arg "get";
    a.data.(i)

  let set a i v =
    if i < 0 || i >= a.size then invalid_arg "set";
    a.data.(i) <- v

  let resize a s =
    if s <= a.size then
      Array.fill a.data s (a.size - s) a.default
    else begin
      let n = Array.length a.data in
      if s > n then begin
	let n' = max (2 * n) s in
	let a' = Array.make n' a.default in
	Array.blit a.data 0 a' 0 a.size;
	a.data <- a'
      end
    end;
    a.size <- s

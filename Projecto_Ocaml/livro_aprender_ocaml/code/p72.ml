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

(* Programme 72 page 298
   Structure union-find *)

type t = {
  rank: int array;
  link: int array;
}

let create n =
  { rank = Array.make n 0;
    link = Array.init n (fun i -> i) }

let rec find t i =
  let p = t.link.(i) in
  if p = i then
    i
  else begin
    let r = find t p in
    t.link.(i) <- r;
    r
  end

let union t i j =
  let ri = find t i in
  let rj = find t j in
  if ri <> rj then begin
    if t.rank.(ri) < t.rank.(rj) then
      t.link.(ri) <- rj
    else begin
      t.link.(rj) <- ri;
      if t.rank.(ri) = t.rank.(rj) then
	t.rank.(ri) <- t.rank.(ri) + 1
    end
  end

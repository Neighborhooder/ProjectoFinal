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

(* Programme 46 page 223
   Recherche dans une table de hachage associative *)

  let find x h =
    let rec lookup = function
      | [] -> raise Not_found
      | (k, v) :: _ when X.equal x k -> v
      | _ :: b -> lookup b
    in
    let i = bucket_of x h in
    lookup h.buckets.(i)

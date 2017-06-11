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

(* Programme 44 page 219
   Suppression dans une table de hachage *)

  let remove x h =
    let i = bucket_of x h in
    let b = h.buckets.(i) in
    if mem_bucket x b then begin
      h.size <- h.size - 1;
      h.buckets.(i) <- List.filter (fun y -> not (X.equal y x)) b
    end

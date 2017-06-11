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

(* Programme 42 page 218
   Recherche dans une table de hachage *)

  let bucket_of x h = 
    (X.hash x) mod (Array.length h.buckets)

  let mem_bucket x b =
    List.exists (X.equal x) b

  let mem x h =
    let i = bucket_of x h in
    mem_bucket x h.buckets.(i)

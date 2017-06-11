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

(* Programme 28 page 179
   Extraction d'une sous-corde *)

  let rec mksub start stop t =
    if start = 0 && stop = length t then
      t
    else match t with
      | Str (s, ofs, _) ->
	  Str (s, ofs+start, stop-start)
      | App (t1, t2, _) ->
	  let n1 = length t1 in
	  if stop <= n1 then mksub start stop t1
	  else if start >= n1 then mksub (start-n1) (stop-n1) t2
	  else mksub start n1 t1 ++ mksub 0 (stop-n1) t2

  let sub t ofs len =
    let stop = ofs + len in
    if ofs < 0 || len < 0 || stop > length t then invalid_arg "sub";
    if len = 0 then empty else mksub ofs stop t

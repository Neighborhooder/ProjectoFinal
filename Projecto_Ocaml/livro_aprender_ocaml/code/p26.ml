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

(* Programme 26 page 173
   Opérations élémentaires sur les cordes *)

module Make(X : STRING) : (ROPE with module S = X) = struct

  module S = X

  type char = S.char

  type t =
    | Str of S.t * int * int
    | App of t * t * int

  let empty = Str (S.empty, 0, 0)

  let length = function
    | Str (_,_,n)
    | App (_,_,n) -> n

  let of_string s = Str (s, 0, S.length s)

  let make n c = of_string (S.make n c)

  let rec unsafe_get t i = match t with
    | Str (s, ofs, _) ->
	S.get s (ofs + i)
    | App (t1, t2, _) ->
	let n1 = length t1 in
	if i < n1 then unsafe_get t1 i else unsafe_get t2 (i - n1)

  let get t i =
    if i < 0 || i >= length t then invalid_arg "get";
    unsafe_get t i

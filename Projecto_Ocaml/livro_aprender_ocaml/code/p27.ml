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

(* Programme 27 page 176
   Concaténation de deux cordes *)

  let small_length = 256

  let append_string s1 ofs1 len1 s2 ofs2 len2 =
    Str (S.append (S.sub s1 ofs1 len1) (S.sub s2 ofs2 len2),
         0, len1 + len2)

  let append t1 t2 = match t1, t2 with
    | Str (_,_,0), t | t, Str (_,_,0) ->
	t
    | Str (s1, ofs1, len1), Str (s2, ofs2, len2)
     when len1 <= small_length && len2 <= small_length ->
	append_string s1 ofs1 len1 s2 ofs2 len2
    | App (t1, Str (s1, ofs1, len1), _), Str (s2, ofs2, len2)
     when len1 <= small_length && len2 <= small_length ->
	App (t1, append_string s1 ofs1 len1 s2 ofs2 len2,
             length t1 + len1 + len2)
    | Str (s1, ofs1, len1), App (Str (s2, ofs2, len2), t2, _)
     when len1 <= small_length && len2 <= small_length ->
	App (append_string s1 ofs1 len1 s2 ofs2 len2, t2,
             len1 + len2 + length t2)
    | t1, t2 ->
	App (t1, t2, length t1 + length t2)

  let (++) = append

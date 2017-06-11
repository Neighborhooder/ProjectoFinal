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

(* Programme 25 page 171
   Signature des cordes *)

module type ROPE = sig
  module S : STRING
  include STRING with type char = S.char
  val of_string : S.t -> t
  val set : t -> int -> char -> t
  val delete_char : t -> int -> t
  val insert_char : t -> int -> char -> t
  val insert : t -> int -> t -> t
end

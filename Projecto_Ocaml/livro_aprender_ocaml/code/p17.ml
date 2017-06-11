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

(* Programme 17 page 154
   Signature minimale pour des tableaux redimensionnables *)

module type ResizeableArray = sig
  type 'a t
  val length : 'a t -> int
  val make : int -> 'a -> 'a t
  val resize : 'a t -> int -> unit
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end

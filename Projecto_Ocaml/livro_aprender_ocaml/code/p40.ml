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

(* Programme 40 page 216
   Signature pour des ensembles impératifs *)

module type ImperativeSet = sig
  type elt
  type t
  val create : unit -> t
  val add : elt -> t -> unit
  val mem : elt -> t -> bool
  val remove : elt -> t -> unit
  val cardinal : t -> int
end

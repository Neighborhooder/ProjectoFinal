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

(* Programme 67 page 280
   Signature minimale pour des graphes où les sommets sont des entiers *)

  type vertex = int
  type t

  val create : int -> t
  val nb_vertex : t -> int

  val mem_edge : t -> vertex -> vertex -> bool
  val add_edge : t -> vertex -> vertex -> unit
  val remove_edge : t -> vertex -> vertex -> unit

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit
  val iter_edge : (vertex -> vertex -> unit) -> t -> unit

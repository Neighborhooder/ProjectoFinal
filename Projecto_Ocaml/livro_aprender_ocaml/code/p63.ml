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

(* Programme 63 page 264
   Structure de tas impérative (1/2) *)

module type OrderedWithDummy = sig
  type t
  val compare: t -> t -> int
  val dummy: t
end

module Make(X: OrderedWithDummy)(A: ResizeableArray)
  : ImperativePriorityQueue with type elt = X.t =
struct
  type elt = X.t
  type t = elt A.t

  let create () = 
    A.make 0 X.dummy

  let is_empty h = 
    A.length h = 0

  let get_min h =
    if A.length h = 0 then invalid_arg "get_min";
    A.get h 0

  let rec move_up h x i =
    if i = 0 then A.set h i x else
      let fi = (i - 1) / 2 in
      let y = A.get h fi in
      if X.compare y x > 0 then begin
        A.set h i y; 
	move_up h x fi
      end else
        A.set h i x

  let add x h =
    let n = A.length h in A.resize h (n + 1); move_up h x n

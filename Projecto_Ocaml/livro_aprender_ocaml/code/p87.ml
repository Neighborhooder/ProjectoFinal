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

(* Programme 87 page 352
   Hash-consing (code) *)

  type tree = E | N of int * tree * char * tree

  let empty =
    E

  let unique = function
    | E -> 0
    | N (u, _, _, _) -> u

  module X = struct
    type t = tree
    let hash = function
      | E ->
          0
      | N (_, l, c, r) ->
          (19 * (19 * unique l + Char.code c) + unique r)
          land max_int
    let equal t1 t2 = match t1, t2 with
      | E, E -> true
      | N (_, l1, c1, r1), N (_, l2, c2, r2) ->
          l1 == l2 && c1 == c2 && r1 == r2
      | _ -> false
  end
  module W = Weak.Make(X)
  let nodes = W.create 5003

  let node =
    let cpt = ref 1 in
    fun l c r ->
      let n0 = N (!cpt, l, c, r) in
      let n = W.merge nodes n0 in
      if n == n0 then incr cpt;
      n

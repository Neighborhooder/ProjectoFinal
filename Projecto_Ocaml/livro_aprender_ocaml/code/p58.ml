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

(* Programme 58 page 255
   Files impératives à l'aide de listes chaînées *)

type 'a cell = { elt : 'a; mutable next : 'a cell }

type 'a t = 'a cell option ref

let create () =
  ref None

let is_empty q =
  !q = None

let push x q = match !q with
  | None ->
      let rec c = { elt = x; next = c } in
      q := Some c
  | Some last ->
      let c = { elt = x; next = last.next } in
      last.next <- c;
      q := Some c

let pop q = match !q with
  | None ->
      invalid_arg "pop"
  | Some last when last.next == last ->
      q := None;
      last.elt
  | Some last ->
      let first = last.next in
      last.next <- first.next;
      first.elt

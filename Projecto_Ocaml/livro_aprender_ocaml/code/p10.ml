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

(* Programme 10 page 78
   Casse-briques *)

open Graphics

let left = 0.
let right = 300.
let down = 0.
let up = 200.

let ball = 5
let paddle = 50
let thick = 8

let gray = rgb 220 220 220

let init () =
  let s = Printf.sprintf " %dx%d" (truncate right) (truncate up) in
  open_graph s;
  auto_synchronize false

let clear () =
  set_color gray;
  fill_rect 0 0 (truncate right) (truncate up)

let get_paddle_pos () =
  let x = fst (mouse_pos ()) in
  max 0 (min x (truncate right - paddle))

let game x y =
  clear ();
  set_color black;
  fill_circle (truncate x) (truncate y) ball;
  let x = get_paddle_pos () in
  fill_rect x 0 paddle thick;
  synchronize ();
  x

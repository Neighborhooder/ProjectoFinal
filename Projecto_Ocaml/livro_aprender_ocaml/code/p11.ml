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

(* Programme 11 page 82
   Casse-briques *)

let bounce (x, y) (vx, vy) xp =
  let vx =
    if x <= Draw.left || x >= Draw.right then -. vx else vx in
  let vy =
    if y <= float Draw.thick && x >= xp &&
       x <= xp +. float Draw.paddle || y >= Draw.up
    then -. vy else vy
  in
  (vx, vy)

let new_position (x, y) (vx, vy) = x +. vx, y +. vy

let rec play (x, y) (vx, vy) =
  if y <= Draw.down then failwith "game over";
  let xp = Draw.game x y in
  let vx, vy = bounce (x, y) (vx, vy) (float xp) in
  let x', y' = new_position (x, y) (vx, vy) in
  play (x', y') (vx, vy)

let () =
  Draw.init();
  let speed = 0.1 in
  let vx = speed *. Random.float 1. in
  let vy = speed *. Random.float 1. in
  play (Draw.right /. 2., float Draw.thick) (vx, vy)

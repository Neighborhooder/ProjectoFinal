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

(* Programme 13 page 90
   Une tortue Logo *)

open Graphics


module type ANGLE = sig
  type t
  val of_degrees: float -> t
  val add: t -> t -> t
  val cos: t -> float
  val sin: t -> float
end

module Turtle(A: ANGLE) = struct

  let draw = ref true
  let pen_down () = draw := true
  let pen_up   () = draw := false

  let angle = ref (A.of_degrees 0.)
  let rotate_left d = angle := A.add !angle (A.of_degrees d)
  let rotate_right d = rotate_left (-. d)

  let tx = ref 400.
  let ty = ref 300.
  let () = open_graph " 800x600"; moveto 400 300; set_line_width 2

  let advance d =
    tx := !tx +. d *. A.cos !angle;
    ty := !ty +. d *. A.sin !angle;
    if !draw then lineto (truncate !tx) (truncate !ty)
             else moveto (truncate !tx) (truncate !ty)

end

module Angle : ANGLE = struct
  type t = float
  let add = (+.)
  let pi_over_180 = atan 1. /. 45.
  let of_degrees d = d *. pi_over_180
  let cos = Pervasives.cos
  let sin = Pervasives.sin
end

module T = Turtle(Angle)


let square d =
  for k = 1 to 4 do
    T.advance d;
    T.rotate_left 90.
  done
let squares d a =
  for k = 1 to truncate (360. /. a) do
    square d; T.rotate_left a
  done

let () = square 100.

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

(* Programme 3 page 26
   Dessin d'une cardioïde *)

open Graphics

let () = open_graph " 300x200"

let () =
  moveto 200 150;
  Graphics.set_color red;
  set_line_width 2;
  for i = 0 to 100 do
    let th = atan 1. *. float i /. 25. in
    let r = 50. *. (1. -. sin th) in
    lineto (150 + truncate (r *. cos th))
           (150 + truncate (r *. sin th))
  done;
  set_color black;
  for i = 100 to 200 do
      let th = atan 1. *. float i /. 25. in
      let r = 50. *. (1. -. sin th) in
      lineto (150 + truncate (r *. cos th))
             (150 + truncate (r *. sin th))
    done;
  ignore (read_key ())

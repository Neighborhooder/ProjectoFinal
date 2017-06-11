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

(* Programme 82 page 331
   Calcul modulo $m$ (multiplication et division) *)

  let mul x y =
    let r = ref 0 in
    for i = Sys.word_size - 4 downto 0 do
      r := add !r !r;
      if x land (1 lsl i) <> 0 then r := add !r y
    done;
    !r

  let div x y =
    let u, _, g = extended_gcd y m in
    if g <> 1 then invalid_arg "div";
    mul x (of_int u)

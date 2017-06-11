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

(* Programme 5 page 37
   Crible d'Ératosthène *)

let max = read_int ()

let prime = Array.make (max + 1) true

let () =
  prime.(0) <- false;
  prime.(1) <- false;
  let limit = truncate (sqrt (float max)) in
  for n = 2 to limit do
    if prime.(n) then begin
      let m = ref (n * n) in
      while !m <= max do
	prime.(!m) <- false;
	m := !m + n
      done
    end
  done

let () =
  for n = 2 to max do
    if prime.(n) then Printf.printf "%d\n" n
  done

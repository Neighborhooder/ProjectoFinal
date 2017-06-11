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

(* Programme 9 page 69
   Conversion d'entiers en base quelconque *)

let base = int_of_string Sys.argv.(1)

let list_of_string s =
  let digits = ref [] in
  for i = 0 to String.length s - 1 do
    digits := s.[i] :: !digits
  done;
  !digits

let digit_of_char c = 
  match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'Z' -> 10 + Char.code c - Char.code 'A'
    | c -> Printf.eprintf "invalid character %c\n" c; exit 1

let check_digit d =
  if d < 0 || d >= base then begin
    Printf.eprintf "invalid digit %d\n" d; exit 1
  end

let () =
  while true do
    let s = read_line () in
    let cl = list_of_string s in
    let dl = List.map digit_of_char cl in
    List.iter check_digit dl;
    let v = List.fold_right (fun d acc -> d + base * acc) dl 0 in
    Printf.printf " -> %d\n" v
  done

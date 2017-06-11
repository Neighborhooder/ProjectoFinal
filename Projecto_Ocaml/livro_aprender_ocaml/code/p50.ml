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

(* Programme 50 page 229
   Ajout et suppression dans un arbre de préfixes *)

  let rec add x t =
    match x with
    | [] ->
        if t.word then t else { t with word = true }
    | i::l ->
        let b = try M.find i t.branches with Not_found -> empty in
        { t with branches = M.add i (add l b) t.branches }

  let rec remove x t =
    match x with
    | [] ->
        { t with word = false }
    | i::l ->
        try
          let s = remove l (M.find i t.branches) in
          let new_branches =
            if is_empty s then M.remove i t.branches
            else M.add i s t.branches
          in
          { t with branches = new_branches }
        with Not_found -> t

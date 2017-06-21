(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2007                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Kruskal and Prim tests *)

open Printf
open Graph

(* command line *)
let v_ = ref 300
let e_ = ref 1000
let seed_ = ref None
let debug_ = ref false

let arg_spec =
  ["-v", Arg.Int (fun i -> v_ := i),
   " <int>  number of vertices";
   "-e", Arg.Int (fun i -> e_ := i),
   " <int>  number of edges";
   "-seed", Arg.Int (fun n -> seed_ := Some n),
   " <int>  random seed";
   "--debug", Arg.Set debug_, "set the debug flag";
  ]
let () = Arg.parse arg_spec (fun _ -> ()) "usage: color <options>"

let v = !v_
let e = !e_
let debug = !debug_

let seed = match !seed_ with
  | None -> Random.self_init (); Random.int (1 lsl 29)
  | Some s -> s
let () = Format.printf "seed = %d@." seed; Random.init seed

(* undirected graphs with integer coordinates and integer labels on edges *)

module IntInt = struct
  type t = int * int
end
module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = 0
end
module V = struct
  type t = int
end

module E = struct
  type t = int
  type label = int
  let compare = Pervasives.compare
  let default = (0)
end

module GI = Imperative.Graph.AbstractLabeled(V)(E)

module G = Imperative.Graph.AbstractLabeled(Int)(Int)

module R = Rand.I(G)
module RI = Rand.I(GI)


module W = struct
  type label = G.E.label
  type edge = G.E.t
  type t = int
  let weight _ = 10
  let zero = 0
  let add = (+)
  let compare = compare
end

module WI = struct
  type label = G.E.label
  type edge = G.E.t
  type t = G.E.label
  let weight x = G.E.label x
  let zero = 0
  let add = (+)
  let compare = compare
end

module Time = struct

  open Unix

  let utime f x =
    let u = (times()).tms_utime in
    let y = f x in
    let ut = (times()).tms_utime -. u in
    y, ut

  (* runs f 5 times, removes minimum and maximum timings, and
     returns the mean of the remaining three timings *)
  let time5 f x =
    let t = Array.init 5 (fun _ -> snd (utime f x)) in
    if debug then Array.iter (fun x -> Printf.printf "%2.2f\n" x) t;
    Array.sort Pervasives.compare t;
    (t.(1) +. t.(2) +. t.(3)) /. 3.

  let print f x =
    let (y,ut) = utime f x in
    printf "user time: %2.2f@." ut;
    y

end

module P1 = Kruskal.Make(G)(WI)
module P2 = Prim.Make(G)(WI)
module P3 = Boruvka.Make(GI)(WI)

let testp g = Time.time5 P1.spanningtree g
let kruskall g = P1.spanningtree g
let testk g = Time.time5 P2.spanningtree g
let prim g = P2.spanningtree g
let testb g = Time.time5 P3.spanningtree g
let boruvkaa g = P3.spanningtree g

let test nb_v nb_e =
  Printf.printf "Execution time v=%d - e=%d\n" nb_v nb_e;
  let g = R.graph ~v:nb_v ~e:nb_e () in
  let gi = RI.graph ~v:nb_v ~e:nb_e () in
  let get_weight_of_MST e = List.fold_left (fun acc i -> acc + (GI.E.label i)) 0 e in
  let get_weight_of_MSTs e = List.fold_left (fun acc i -> acc + (G.E.label i)) 0 e in
  let resp = testp g in
  Printf.printf "PRIM : %2.2fs\n" resp;
  Printf.printf "weight with kruskal : %d %!" (get_weight_of_MSTs (kruskall g));
  let resk = testk g in
  Printf.printf "KRUSKAL : %2.2fs\n%!" resk;
  Printf.printf "weight with prim : %d %!" (get_weight_of_MSTs (prim g));
  let resz = testb gi in
  Printf.printf "BORUVKA : %2.2fs\n%!" resz;
  Printf.printf "weight with boruvka : %d %!" (get_weight_of_MST (boruvkaa gi))

let () = test v e
(*

Local Variables:
compile-command: "make -C .. bin/compare_prim_kruskal.opt"
End:
*)

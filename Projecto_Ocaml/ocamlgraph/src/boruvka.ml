(**************************************************************************)
(*                                                                        *)
(*  Ocamlgraph: a generic graph library for OCaml                         *)
(*  Copyright (C) 2004-2010                                               *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* $Id: kruskal.ml,v 1.5 2005-06-30 10:48:55 filliatr Exp $ *)

module type UNIONFIND = sig
  type elt
  type t
  val init : elt list -> t
  val find : elt -> t -> elt
  val union : elt -> elt -> t -> unit
end
  (*UNIAO DE SETS*)
module type UNIONFINDBORUVKA = sig
  type elt
  type t
  val init : elt list -> t
  val find : elt -> t -> elt
  val union : elt -> elt -> t -> unit
end



module type G = sig
  type t
  module V : Sig.COMPARABLE


  module E : sig
    type t
    type label
    val label : t -> label
    val dst : t -> V.t
    val src : t -> V.t
  end
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges_e : (E.t -> unit) -> t ->  unit
end

module Generic
    (G: G)
    (W : Sig.ORDERED_TYPE with type t = G.E.label)
    (UF: UNIONFIND with type elt = G.V.t)  =
struct

  module SetOfList (E : Set.OrderedType) = struct
                       module S = Set.Make(E)
                       let set_of_list li =
                         List.fold_left (fun set elem -> S.add elem set) S.empty li

  end
  module SoL = SetOfList(G.V)

  let spanningtree g =
    let list_of_vertex = G.fold_vertex (fun acc elem -> acc::elem) g [] in
    (* let vertex_set = SoL.set_of_list list_of_vertex in (*mesmo que [1,2,3] -> {1,2,3} ----- queremos [{1},{2},{3}]*) *)
    let list_of_sets = List.fold_left (fun acc i -> (SoL.S.singleton i)::acc ) [] list_of_vertex in

    let l = ref [] in
    let assoc_edges vertex = G.iter_edges_e (fun elem->
        if ((G.V.compare (G.E.dst elem) vertex) = 0) then
              l := elem :: !l;
        if ((G.V.compare (G.E.src elem) vertex) = 0) then
            l := elem :: !l;
      ) g;
      List.sort (fun e e'-> W.compare (G.E.label e) (G.E.label e')) !l in
    (* let cheaper vertex = List.hd (assoc_edges vertex) in *)
    (* let set_to_unify = ref (List.hd list_of_sets) in *)
    let find_in_list_of_sets vertex = List.fold_left (fun set acc -> if(SoL.S.mem vertex set) then set else acc) (SoL.S.empty) list_of_sets in

    let assoc_edges_of_all set = SoL.S.fold (fun i acc -> let edges = assoc_edges i in List.append acc edges) set [] in

    let list_of_all_assoc_edges set = List.sort (fun e e'-> W.compare (G.E.label e) (G.E.label e')) (assoc_edges_of_all set) in


    let remove_elt e l =
      let rec go l acc = match l with
        | [] -> List.rev acc
        | x::xs when e = x -> go xs acc
        | x::xs -> go xs (x::acc)
      in go l []
    in
    let remove_duplicates l =
    let rec go l acc = match l with
      | [] -> List.rev acc
      | x :: xs -> go (remove_elt x xs) (x::acc)
    in go l [] in


    let union set1 set2 = SoL.S.union (find_in_list_of_sets set1) (find_in_list_of_sets set2) in

    let list_of_sets_ref = ref list_of_sets in
    let s = ref [] in
    let unify = List.fold_left (fun acc set ->
        let list_of_edges = (list_of_all_assoc_edges set) in
               List.iter (fun i -> if (SoL.S.mem (G.E.src i) set) then ()
                           else let union = (union (G.E.src i) (G.E.dst i)
                                            ) in
               list_of_sets_ref := List.filter (fun elem ->
                                not( SoL.S.equal elem (find_in_list_of_sets (G.E.src i))) ) !list_of_sets_ref;
               list_of_sets_ref := union::!list_of_sets_ref;
               s := i::!s;
               s := remove_duplicates !s
                         ) list_of_edges
      ) () !list_of_sets_ref in
    unify;
    !s
end



    (* let list_from_set = SoL.S.fold (fun elem set -> elem::set) vertex_set [] in *)
    (* let uf = UF.init (SoL.S.elements list_of_sets) in *)




(*
  let create_set_of_vertex  g =
    let s = S.empty in
    let set_of_list  = G.fold_vertex (fun v a -> (S.add v s)::a ) g [] in
    ver
    List.fold_left (fun ve acc -> (G.E.label ve) ^ acc ) "" ver
     S.iter (fun el -> (S.partition (fun elt -> S.mem el s) s)) s



  let spanningtree g =
    let vertices = G.fold_vertex (fun v a -> v :: a) g [] in
    let uf = UF.init vertices in
    let edges =
      let l = ref [] in
      G.iter_edges_e (fun e -> l := e :: !l) g;
      List.sort (fun e e'-> W.compare (G.E.label e) (G.E.label e')) !l
    in
    let s = ref [] in
    let cover e =
      let u, v = G.E.src e, G.E.dst e in
      if G.V.compare (UF.find u uf) (UF.find v uf) <> 0 then begin
        UF.union u v uf;
        s := e :: !s
      end
    in
    List.iter cover edges;
    !s

end
*)
module Make(G: G)(W : Sig.ORDERED_TYPE with type t=G.E.label) =
  Generic(G)(W)(Unionfind.Make(G.V))

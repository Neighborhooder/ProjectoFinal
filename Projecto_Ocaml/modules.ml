open Sys
open Format
open Graph


module Ford_Fulkerson
 (G : G_FORD_FULKENSON)
 (F : FLOW with type label = G.E.label) :
  sig
    val maxflow : G.t ->  G.V.t -> G.V.t -> (G.E.t -> F.t) * F.t
  end

module Goldberg
(G : G_GOLDBERG)
(F : FLOW with type label = G.E.label) :
sig
  val maxflow :
   G.t -> G.V.t -> G.V.t -> (G.E.t -> F.t) * F.t
end

module type G_GOLDBERG = sig
  type t
  module V : sig type t (* ... *) end
  module E : sig type t (* ... *) end
  val iter_vertex : (V.t -> unit) -> t -> unit
  val iter_edges : (E.t -> unit) -> t -> unit
  (* .... *)
end

module type G_FORD_FULKENSON = sig
  type t
  module V : sig type t (* ... *) end
  module E : sig type t (* ... *) end
  val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
  val iter_pred_e : (E.t -> unit) -> t -> V.t -> unit
end

module type FLOW = sig
  type label
  type t
  val flow : label -> t
  val max_capacity : label -> t
  val min_capacity : label -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val zero : t
  val compare : t -> t -> int
end

module Flow = struct
  type label = int
  type t = int
  let max_capacity x = x
  let min_capacity _ = 0
  let flow _ = 0
  let add = (+)
  let sub = (-)
  let compare = compare
  let zero = 0
end

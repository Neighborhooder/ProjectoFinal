open Graph

module Make_graph
    (G : Sig.G with type V.label =  int)
    (B : Builder.S with module G = G)=
struct
  module W = struct
  type label = G.E.label
  type t = int
  type edge = G.E.t
  let weight _ = 1
  let zero = 0
  let add = (+)
  let compare = compare
  end

  module Kruskal_test = Kruskal.Make(G)
  module Prim_test = Prim.Make(G)
end

module V = struct
  type t = int
end
module E = struct
  type t = int * int
  let compare = Pervasives.compare
  let default = (0,0)
end

module G = Imperative.Graph.AbstractLabeled(V)(E)

module My_maze = Make_graph(G)(Builder.I(G))

let g = G.create ()

let list_nodes = ['A';'B';'C';'D';'E';'F';'G';'H']

let add_vertex = List.iter (fun i -> let v = G.V.create (Char.code i) in G.add_vertex g v) list_nodes

let print_graph = G.iter_vertex (fun i -> print_int (G.V.label i)) g

let () = print_graph

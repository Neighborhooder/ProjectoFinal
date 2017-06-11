open Graph

module V = struct
  type t = int
end

module E = struct
  type t = int
  let compare = Pervasives.compare
  let default = 0
end

module MakeGraph
    (G : Sig.G with type V.label = int)
    (B : Builder.S with module G = G) =
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


end



module G = Imperative.Graph.AbstractLabeled(V)(E)

module My_module = MakeGraph(G)(Builder.I(G))

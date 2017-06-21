(* Boruvka's Algorithm *)

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
  val find_edge : t -> V.t -> V.t -> E.t
  val succ_e : t -> V.t -> E.t list
  val pred_e : t -> V.t -> E.t list
  val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges_e : (E.t -> unit) -> t ->  unit
end

module Generic
    (G: G)
    (W : Sig.ORDERED_TYPE with type t = G.E.label) =
struct

  module SetOfVertex (E : Set.OrderedType) = struct
    module S = Set.Make(E)
    module SS = Set.Make(S)
  end
  module SoS = SetOfVertex(G.V)

      (*
        ------ Pseudocódigo -------
        0) Input : Um grafo conexo G em que todas as arestas têm pesos distintos
        1) Inicializar uma floresta T para que seja um conjunto de árvores de 1 vértice apenas, para cada vértice do grafo
        2) Enquanto T tiver mais do que 1 componente:
        3) Para cada elemento C de T:
        4) Inicializar um conjunto vazio de arestas S
        5) Para cada vértice v de C:
        6) Encontrar a aresta mais barata de v para um vértice fora de C, e adiciona-lo a S
        7) Adicionar a aresta mais barata de S a T
        8) Combinar árvores ligadas por arestas, de forma a obter componentes maiores
        9) Output : T é a arvore minimal de G

        *)


  let spanningtree g =
    (* Criamos uma lista de vértices *)
    let list_of_vertex = G.fold_vertex (fun acc elem -> acc::elem) g [] in
    (* Inicializar uma floresta T para que seja um conjunto de árvores de 1 vértice apenas, para cada vértice do grafo  *)
    let set_of_sets = ref (List.fold_left (fun acc i -> (SoS.SS.add (SoS.S.singleton i) acc)) (SoS.SS.empty) list_of_vertex) in
    (* arestas associadas a um vértice *)
    let assoc_edges vertex = (G.succ_e g vertex)@(G.pred_e g vertex) in
    (* retorna um conjunto correspondente a um vertice *)
    let find_in_set_of_sets vertex = SoS.SS.fold (fun acc set -> if (SoS.S.mem vertex set) then set else acc) !set_of_sets (SoS.S.empty) in
    (* retorna as arestas associadas a um set de vértices *)
    let assoc_edges_of_set set = SoS.S.fold (fun vertex acc -> let edges = assoc_edges vertex in acc@edges) set [] in
    (* ordena as arestas de um set por peso *)
    let assoc_edges_ordered set = List.sort (fun e e'-> W.compare (G.E.label e) (G.E.label e')) (assoc_edges_of_set set) in
    (* une dois conjuntos associados a dois vertices *)
    let union vertex1 vertex2 = SoS.S.union (find_in_set_of_sets vertex1) (find_in_set_of_sets vertex2) in
    (* s corresponde ás arestas da spanningtree final *)
    let s = ref [] in
    (* Enquanto T tiver mais do que 1 componente: *)
    while (SoS.SS.cardinal !set_of_sets > 1) do (
    (* Encontrar a aresta mais barata de v para um vértice fora de C, e adiciona-lo a S *)
          let edges_to_add = SoS.SS.fold(fun set acc ->
                                      try (* retorna a aresta mais barata de um set para qualquer outro set  *)
                                                    (List.find (fun edge -> (* verificamos se o vértice de origem e destino se encontram no mesmo conjunto *)
                                                                  not((SoS.S.mem (G.E.src edge) set) && (SoS.S.mem (G.E.dst edge) set))
                                                                  &&  (* verificamos se a aresta ja se encontra na lista que estamos a iterar ou na spanningtree *)
                                                                  not((List.mem (G.find_edge g (G.E.dst edge) (G.E.src edge)) acc)
                                                                      || (List.mem (G.find_edge g (G.E.dst edge) (G.E.src edge)) !s))
                                                                  && (* repetimos a verificaçao anterior, para os casos aresta = (A,B) && aresta (B,A) *)
                                                                  not((List.mem (G.find_edge g (G.E.src edge) (G.E.dst edge)) acc)
                                                                      || (List.mem (G.find_edge g (G.E.src edge) (G.E.dst edge)) !s))
                                                                  && (* fazemos uma verificação se a lista actual ja tem o vertice de origem e chegada *)
                                                                  not(List.exists (fun i -> ((G.V.compare (G.E.src i) (G.E.src edge)) = 0 )) acc)
                                                                  &&
                                                                  not(List.exists (fun i -> ((G.V.compare (G.E.dst i) (G.E.dst edge)) = 0 )) acc)
                                                                  &&
                                                                  not(List.exists (fun i -> ((G.V.compare (G.E.dst i) (G.E.src edge)) = 0 )) acc)
                                                                  &&
                                                                  not(List.exists (fun i -> ((G.V.compare (G.E.src i) (G.E.dst edge)) = 0 )) acc)
                                                      )(assoc_edges_ordered set))::acc
                                                  with Not_found -> acc
          ) !set_of_sets []  in

          (* ordenamos a lista de arestas entre sets por peso *)
          let edges_to_add_ordered = List.sort (fun e e'-> W.compare (G.E.label e) (G.E.label e')) edges_to_add in
          (* definimos um conjunto uniao entre dois conjuntos {A}{B}->{A,B} *)
          
            let unify = union (G.E.src (List.hd edges_to_add_ordered)) (G.E.dst (List.hd edges_to_add_ordered)) in
          begin
            (* removemos o conjunto de partida e de chegada *)
              set_of_sets := SoS.SS.remove (find_in_set_of_sets (G.E.src (List.hd edges_to_add_ordered))) !set_of_sets;
              set_of_sets := SoS.SS.remove (find_in_set_of_sets (G.E.dst (List.hd edges_to_add_ordered))) !set_of_sets;
            (* adicionamos ao set a uniao entre o conjunto de partida e chegada *)
              set_of_sets := SoS.SS.add unify !set_of_sets;
            (* adicionamos a aresta à spanningtree final *)
              s:= (List.hd edges_to_add_ordered)::!s

            end

    ) done;
    (* ordena a spanningtree final por peso e devolve-a *)
    List.sort (fun e e'-> W.compare (G.E.label e) (G.E.label e')) !s
end

module Make(G: G)(W : Sig.ORDERED_TYPE with type t=G.E.label) =
  Generic(G)(W)

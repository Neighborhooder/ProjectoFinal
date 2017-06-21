open Graph

module MakeMaze
    (G : Sig.G with type V.label = int * int)
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

  module Dij = Path.Dijkstra(G)(W)

end

(* module GI = Imperative.Graph.Abstract(struct type t = int * int end) *)

type vertex = int * int
module V = struct
  type t = vertex
end
type edge = vertex * vertex
module E = struct
  type t = edge
  let compare = Pervasives.compare
  let default = ((0,0),(0,0))
end

module G = Imperative.Graph.AbstractLabeled(V)(E)

module My_maze = MakeMaze(G)(Builder.I(G))






let g = G.create ()

let m = 27
let n = 27




let nodes =
  let new_node i j =
    let v = G.V.create (i, j) in G.add_vertex g v;
    v
  in
  Array.init n (fun i -> Array.init m (new_node i))

let node i j = nodes.(i).(j)




let () =
  for i = 0 to (n-1) do
    let x = read_line () in
    for j = 0 to (m-1) do
      match x.[j] with
      | '1'       -> G.remove_vertex g nodes.(i).(j)
      | '2' as ch ->  G.Mark.set (node i j) (Char.code ch)
      | '3' as ch ->  G.Mark.set (node i j) (Char.code ch)
      | _ as ch -> G.Mark.set (node i j) (Char.code ch)
    done
  done



let add_edges =
  for i = 0 to (n-1) do for j = 0 to (m-1) do
      (if i > 0 then
         if (((G.Mark.get nodes.(i-1).(j)) = (Char.code '0') || (G.Mark.get nodes.(i-1).(j)) = (Char.code '2') ||(G.Mark.get nodes.(i-1).(j)) = (Char.code '3')) && ((G.Mark.get nodes.(i).(j)) = (Char.code '0'))) then
           let v = G.E.create (node i j) ((i,j),((i-1),j)) (node (i-1) j) in
           G.add_edge_e g v

      );

      (if j > 0 then
         if (((G.Mark.get nodes.(i).(j-1)) = (Char.code '0') ||(G.Mark.get nodes.(i).(j-1)) = (Char.code '2') || (G.Mark.get nodes.(i).(j-1)) = (Char.code '3')  )&& ((G.Mark.get nodes.(i).(j)) = (Char.code '0')))  then
           let v = G.E.create (node i j) ((i,j),(i,(j-1))) (node i (j-1)) in
         G.add_edge_e g v

      )
    done;
  done

let (lo,liy) = My_maze.Dij.shortest_path g (node 2 1) (node 24 25)

let printlabel ((i,j),(i2,j2)) = Printf.sprintf "(%d,%d) -> (%d,%d) -- " i2 j2 i j
let printlist (_,_) = List.fold_left (fun acc x -> acc ^  printlabel (G.E.label x)) "Algoritmo de Dijkstra " lo


module W = struct
  type label = G.E.label
  type edge = G.E.t
  type t = G.E.label
  let weight _ = 1
  let zero = 0
  let add = (+)
  let compare = compare
end


module P1 = Kruskal.Make(G)(W)
module P2 = Boruvka.Make(G)(W)

let kruskal_test g = P1.spanningtree g
let boruvka_test = P2.spanningtree g

let mst_kruskal = List.fold_left (fun acc x -> acc ^ printlabel (G.E.label x)) "Algoritmo de Kruskal " (kruskal_test g)

let mst_boruvka = List.fold_left (fun acc x -> acc ^ printlabel (G.E.label x)) "Algoritmo de Boruvka " (boruvka_test)


let () =
  print_endline (printlist (lo,liy));
  print_endline (mst_kruskal);
  print_endline (mst_boruvka);
  (* for i = 0 to (n-1) do
    for j = 0 to (m-1) do
      Format.printf "%c" (Char.chr(G.Mark.get (node i j)))
    done;
    Format.printf "\n";
  done; *)
  Format.printf "edges : %d \nvertexs : %d @?" (G.nb_edges g) (G.nb_vertex g);
  Format.printf "@?"

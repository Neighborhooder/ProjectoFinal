open Graph
open Printf
let debug_ = ref false
let debug = !debug_

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


module Make_graph
    (G : Sig.G with type V.label =  int)
    (B : Builder.S with module G = G)=
struct
  module W = struct
    type label = G.E.label
    type t = int
    type edge = G.E.t
    let weight x = G.E.label x
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
  type t = int
  type label = int
  let compare = Pervasives.compare
  let default = (0)
end

module G = Imperative.Graph.AbstractLabeled(V)(E)

module My_maze = Make_graph(G)(Builder.I(G))

let g = G.create ()
let graph = G.create ()


let list_of_cities = ["VC";"VR";"BR";"BG";"PO";"VS";"GU";"AV";"CO";"CB";"LE";"LS";"ST";"PL";"SE";"BE";"FR";"EV"]

let list_edges_of_cities = ["VC-BR";"VC-PO";"BR-PO";"BR-VR";"VR-BG";"VR-GU";"VR-PO";"VR-VS";"VR-AV";"BG-GU";"PO-AV";"PO-CO";"PO-VS";"VS-CO";"VS-GU";"AV-LE";"AV-CO";"CO-LE";"CO-GU";"CO-CB";"CB-LE";"CB-ST";"CB-SE";"CB-PL";"LE-ST";"LE-LS";"ST-LS";"ST-SE";"ST-EV";"ST-PL";"PL-EV";"PL-SE";"SE-FR";"SE-BE";"SE-EV";"EV-BE";"BE-FR";"GU-CB"]

let list_weights_of_cities = [54;73;55;105;118;180;97;94;160;178;74;122;128;102;83;117;62;76;155;138;168;159;259;85;82;150;86;112;121;139;103;198;244;144;98;81;141;100]


let list_nodes = [|'A';'B';'C';'D';'E';'F';'G';'H'|]

let list_edges = ["AD";"AB";"DB";"DE";"EB";"BC";"CE";"GF";"GE";"EF";"DF"]

let list_weights = ["4";"7";"9";"15";"10";"11";"5";"13";"8";"12";"6"]

let nodes =
  let new_node i =
    let v = G.V.create (i) in G.add_vertex g v;
    v
  in
  Array.init 8 (fun i -> new_node i)

let node i = nodes.(i)

let nodes_city =
  let new_node i=
    let v = G.V.create (int_of_string(string_of_int(Char.code (List.nth list_of_cities i).[0]) ^string_of_int(Char.code (List.nth list_of_cities i).[1]))) in G.add_vertex graph v;
    v
  in
  Array.init 18 (fun i -> new_node i)

let node_city i = nodes_city.(i)


let rec get_weight (a,b) cont =
  if ((compare (a,b) ((Char.code(List.nth list_edges cont).[0]),(Char.code(List.nth list_edges cont).[1]))) = 0) then cont
  else get_weight (a,b) (cont+1)


let string_to_edges str =
  let (a,b) = (Char.code(str.[0]),Char.code(str.[1])) in
  let edge = G.E.create (node (a-65)) (int_of_string (List.nth list_weights (get_weight (a,b) 0))) (node (b-65)) in
  G.add_edge_e g edge

let magic_list_edges = List.iter (fun i -> string_to_edges i) list_edges

let city_to_number = List.map (fun i -> int_of_string(string_of_int(Char.code (i.[0])) ^ string_of_int(Char.code (i.[1])))) list_of_cities

let rec get_city a cont = if (( a = (int_of_string((string_of_int(Char.code(List.nth list_of_cities cont).[0]))^(string_of_int(Char.code(List.nth list_of_cities cont).[1])))))) then cont
  else get_city a (cont+1)


let rec get_weight_of_city (a,b) cont =
  if ((compare (a,b) (int_of_string((string_of_int(Char.code(List.nth list_edges_of_cities cont).[0]))^(string_of_int(Char.code(List.nth list_edges_of_cities cont).[1]))),int_of_string((string_of_int(Char.code(List.nth list_edges_of_cities cont).[3]))^(string_of_int(Char.code(List.nth list_edges_of_cities cont).[4]))))) = 0) then cont
  else get_weight_of_city (a,b) (cont+1)

let string_to_edges_of_cities str =
  let (a,b) = ((int_of_string(string_of_int(Char.code str.[0]) ^string_of_int(Char.code str.[1]))),(int_of_string(string_of_int(Char.code str.[3]) ^string_of_int(Char.code str.[4])))) in
  let edge = G.E.create (node_city (get_city a 0)) (List.nth list_weights_of_cities (get_weight_of_city (a,b) 0)) (node_city (get_city b 0)) in
    G.add_edge_e graph edge

let magic_list_edges_of_cities = List.iter (fun i -> string_to_edges_of_cities i) list_edges_of_cities


let label_to_print a = ""^ string_of_int a ^ "->"

(* let print_graph = G.iter_vertex (fun i -> print_endline ("" ^string_of_int(G.V.label i))) g

let print_graph_cities = G.iter_vertex (fun i -> print_endline ("" ^string_of_int(G.V.label i))) graph
 *)

module W = struct
  type label = G.E.label
  type edge = G.E.t
  type t = G.E.label
  let weight x = G.E.label x
  let zero = 0
  let add = (+)
  let compare = compare
end

module Kruskaal = Kruskal.Make(G)(W)

module Prime = Prim.Make(G)(W)

module Boruvka = Boruvka.Make(G)(W)

let kruskal gi = Kruskaal.spanningtree gi
let kruskale = Kruskaal.spanningtree graph
let primer = Prime.spanningtree_from graph
let primee gi = Prime.spanningtree_from gi
let boruvka = Boruvka.spanningtree graph

let get_weight_of_MST e = List.fold_left (fun acc i -> acc + (G.E.label i)) 0 e

let testp g = Time.time5 Kruskaal.spanningtree g
let testk g = Time.time5 Prime.spanningtree g
let testb g = Time.time5 Boruvka.spanningtree g

let _ =
  let resp = testp graph in
  Printf.printf "PRIM : %2.2fs\n" resp;
  let resk = testk graph in
  Printf.printf "KRUSKAL : %2.2fs\n%!" resk;
  let resz = testb graph in
  Printf.printf "BORUVKA : %2.2fs\n%!" resz


let imprime_edges grafo = G.fold_edges_e (fun i acc-> print_int(G.V.label (G.E.src i));print_string(" -> ");print_int(G.V.label (G.E.dst i));print_string(" with weight : ");print_int (G.E.label i);print_string("\n")) grafo ()

let edge_city_to_text e = Char.escaped(Char.chr (int_of_string((Char.escaped (string_of_int(e)).[0])^(Char.escaped (string_of_int(e)).[1])))) ^Char.escaped(Char.chr (int_of_string((Char.escaped (string_of_int(e)).[2])^(Char.escaped (string_of_int(e)).[3]))))

let imprime_edges_cities grafo = G.fold_edges_e (fun i acc-> print_string(edge_city_to_text (G.V.label (G.E.src i)));print_string(" -> ");print_string(edge_city_to_text (G.V.label (G.E.dst i)));print_string(" with weight : ");print_int (G.E.label i);print_string("\n")) grafo ()

let imprime_edges_cities_kruskal =print_string ("Kruskal:\n"); List.iter (fun i ->  print_string(edge_city_to_text (G.V.label (G.E.src i))^ " -> " ^ (edge_city_to_text (G.V.label (G.E.dst i)) ^ " with weight for cities : "^ string_of_int(G.E.label i) ^ "\n"))) kruskale

let order_int_list e = List.sort (fun i i' -> compare i i') e

let print_list_numbers e = List.iter (fun i -> print_string(string_of_int i);print_string(" -> ")) e

let print_list_cities_from_numbers e = List.iter (fun i -> let str = string_of_int i in let (a,b) = ((Char.escaped str.[0]) ^ (Char.escaped str.[1]), (Char.escaped str.[2]) ^ (Char.escaped str.[3])) in print_string (Char.escaped (Char.chr (int_of_string a))^Char.escaped (Char.chr (int_of_string b))^" -- ") ) e

let print_list_cities lista = List.fold_left (fun i acc -> acc ^ " - " ^i) "" lista

let print_list_cities_weights lista_weights = List.iter (fun i ->print_int i;print_string(" - ")) lista_weights

let printlist_edges list_edges = List.fold_left (fun acc i -> acc ^ " - " ^ label_to_print (G.E.label i) ) "" (list_edges g)

let printliste = List.fold_left (fun acc i -> acc ^label_to_print (G.E.label i)^" Nodo : "^string_of_int(G.E.label (i))^" --- ") "" (primee g (node 0))



let imprime_edges_cities_boruvka = print_string ("\nBoruvka:\n"); List.iter (fun i ->  print_string(edge_city_to_text (G.V.label (G.E.src i))^ " -> " ^ (edge_city_to_text (G.V.label (G.E.dst i)) ^ " with weight for cities : "^ string_of_int(G.E.label i) ^ "\n"))) boruvka

(*
let print_weights = List.fold_left (fun acc i -> acc ^ string_of_int (G.E.weight i)) "" (kruskal g) *)

 (* print_graph;
    print_endline(printlist_edges kruskal);
      print_endline(printliste);
  print_endline(print_list_cities list_edges_of_cities );
  print_list_cities_weights list_weights_of_cities;
  print_endline(print_list_cities list_of_cities ^ "\n" );
  imprime_edges g;
print_list_numbers city_to_number;
print_list_cities_from_numbers city_to_number;
      print_graph_cities;*)
(* let () =  imprime_edges_cities_kruskal; *)
let () =
  print_endline("\n");
  (* imprime_edges_cities_boruvka; *)
  print_endline(" Weight of MST with kruskal :" ^ string_of_int(get_weight_of_MST kruskale) ^ " and with Prim :" ^ string_of_int(get_weight_of_MST (primer (node_city 0)))^ " and with Boruvka :" ^ string_of_int(get_weight_of_MST (boruvka)))

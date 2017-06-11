{

  (*i*)
  open Lexing
  open Format
  open Graph
  (*i*)

  (* Graph structure. *)
  module G = struct
    type t = int (** nodes are simple integers *)
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash
  end

  (* Module instantiation to get an imperative implementation
     of a concrete digraph *)
  module Links = Imperative.Digraph.Concrete(G)

  (* Dictionary structure: [page id -> page title]. *)
  let id_to_str_tbl = Hashtbl.create (1 lsl 15)

  (* Dictionary structure: [page title -> page id]. *)
  let str_to_id_tbl = Hashtbl.create (1 lsl 15)

  (* Graph structure for pages link information. *)
  let links_graph = Links.create ~size:(1 lsl 15) ()
}

(*s Parsing rules and graph creation. *)

let uint = ['0'-'9'] ['0'-'9']*
let lalpha = ['a' - 'z']
let ualpha = ['A' - 'Z']
let us = '_'
let title = ualpha (lalpha | us | uint | ualpha)*
let qt = '\''

rule parse_pages = parse
  | '(' (uint as id) ",0," qt (title as title) qt { (*c page info found *)
      let id = (int_of_string id) in
      Hashtbl.add str_to_id_tbl title id;
      Hashtbl.add id_to_str_tbl id title;
      parse_pages lexbuf
    }
  | _ { parse_pages lexbuf }
  | eof { () }

and parse_links = parse
  | '(' (uint as id) ",0," qt (title as title) qt ",0)" { (*c link info found *)
      let id = int_of_string id in
      begin try let id_dest = Hashtbl.find str_to_id_tbl title in
          Links.add_edge links_graph id id_dest
        with Not_found -> () end;
      parse_links lexbuf
    }
  | _   { parse_links lexbuf }
  | eof { () }

{

  (*s Operations over dumps and graph data-structure. *)

  (* Name of pages dump file. *)
  let pages = Sys.argv.(1)

  (* Name of links dump file. *)
  let links = Sys.argv.(2)

  (* Read pages dump information. *)
  let read_pages () =
    let cin = open_in pages in
    let lexbuf = Lexing.from_channel cin in
    parse_pages lexbuf;
    close_in cin

  (* Print [pages.txt] file with pages information. *)
  let print_pages () =
    let cout = open_out "pages.txt" in
    let fmt  = formatter_of_out_channel cout in
    Hashtbl.iter (fun k v -> fprintf fmt "%d %s@\n" k v) id_to_str_tbl;
    fprintf fmt "@.";
    close_out cout

  (* Read links dump information. *)
  let read_links () =
    let cin = open_in links in
    let lexbuf = Lexing.from_channel cin in
    parse_links lexbuf;
    close_in cin

  (* Print a single vertex. *)
  let print_vertex fmt v = fprintf fmt "%d " v

  (* Print the successors of a givent vertex. *)
  let print_succs g fmt v = Links.iter_succ (print_vertex fmt) g v

  (* Print the links graph data-structure. *)
  let print_links () =
    let cout = open_out "links.txt" in
    let fmt  = formatter_of_out_channel cout in
    let print_graph v =
      fprintf fmt "%a [ %a]@\n" print_vertex v (print_succs links_graph) v in
    Links.iter_vertex print_graph links_graph;
    fprintf fmt "@.";
    close_out cout

  (*s Read and dump function. *)
  let read_and_dump () =
    eprintf "Reading pages information... "; read_pages ();
    eprintf "Done!@\nReading links information... "; read_links ();
    eprintf "Done!@\nNow dumpping...@.";
    print_pages (); eprintf "Pages printed@.";
    print_links (); eprintf "Links printed@."

(*s Print path information.*)
  let print_path visited src fmt dest = let len = ref 0 in
    let src_id = Hashtbl.find str_to_id_tbl src in
    let dest_id = Hashtbl.find str_to_id_tbl dest in
    let rec loop node =
      if node = src_id then fprintf fmt "%s ->@ " src
      else begin let pred = Hashtbl.find visited node in
        incr len; loop pred;
        if node = dest_id then
          fprintf fmt "%s@\nPath length: %d@." dest !len
        else let str_node = Hashtbl.find id_to_str_tbl node in
          fprintf fmt "%s ->@ " str_node end in
    loop dest_id

(*s Path functions. We use here only the graph structure provided by
  [OcamlGraph]. We implement ourselves the traversal routines to be able to
  recover the predecessors information. *)

  exception Stop

  (* Breadth-first search (BFS) function. Calling [my_bfs src dest] results in a
  breadth-first traversal over the graph component of [src] (the source
  vertex), stopping earlier if [dest] (the destination vertex) is reached
  during the traversal (raises [Stop] exception). [src] and [dest] are given as
  pages' titles. This is a rather traditional implementation of the BFS
  algorithm: we keep in the [visited] hash-table the vertices reached so far by
  the traversal; we use the queue [q] to store the vertices that remain to be
  treated by our traversal. For each new iteration of the algorithm, we pop the
  element from the top of [q] and proceed as follows: if this vertex is the
  destination vertex, then we are done; if this vertex has not yet been
  visited, we add all of its successors to the end of [q] (function
  [add_bindings]) and store it in the [visited] table. Note that to later be
  able to recover the traversed path, we associate to each vertex [v] in [visited]
  its predecessor [u], i.e., the traversal reached [v] by exploring the edge [u
  -> v]. For [src] we associate itself as its predecessor. *)
  let my_bfs src dest =
    let visited = Hashtbl.create (1 lsl 10) in
    let src_id = Hashtbl.find str_to_id_tbl src in
    let dest_id = Hashtbl.find str_to_id_tbl dest in
    let q = Queue.create () in
    let add_bindings v =
      Links.iter_succ (fun x -> Queue.add (x, v) q) links_graph v in
    add_bindings src_id;
    Hashtbl.add visited src_id src_id;
    let rec bfs () = if not (Queue.is_empty q) then begin
      let (node, pred) = Queue.pop q in
      if node = dest_id then (Hashtbl.add visited node pred; raise Stop);
      if not (Hashtbl.mem visited node) then begin
        try add_bindings node; Hashtbl.add visited node pred
        with Not_found -> () end;
      bfs () end in
    try bfs (); eprintf "Path not found@." with Stop ->
      eprintf "@[%a@]" (print_path visited src) dest

  (* Depth-first search (DFS) function. Calling [my_dfs src dest] results in a
  depth-first traversal over the graph component of [src] (the source
  vertex), stopping earlier if [dest] (the destination vertex) is reached
  during the traversal (raises [Stop] exception). [src] and [dest] are given as
  pages' titles. This is a rather traditional implementation of the DFS
  algorithm: we keep in the [visited] hash-table the vertices reached so far by
  the traversal; we use the stack [s] to store the vertices that remain to be
  treated by our traversal. For each new iteration of the algorithm, we pop the
  element from the top of [s] and proceed as follows: if this vertex is the
  destination vertex, then we are done; if this vertex has not yet been
  visited, we add all of its successors to the top of [s] (function
  [add_bindings]) and store it in the [visited] table. Note that to later be
  able to recover the traversed path, we associate to each vertex [v] in [visited]
  its predecessor [u], i.e., the traversal reached [v] by exploring the edge [u
  -> v]. For [src] we associate itself as its predecessor. *)
  let my_dfs src dest =
    let visited = Hashtbl.create (1 lsl 10) in
    let src_id = Hashtbl.find str_to_id_tbl src in
    let dest_id = Hashtbl.find str_to_id_tbl dest in
    let s = Stack.create () in
    let add_bindings v =
      Links.iter_succ (fun x -> Stack.push (x, v) s) links_graph v in
    add_bindings src_id;
    Hashtbl.add visited src_id src_id;
    let rec dfs () = if not (Stack.is_empty s) then begin
      let (node, pred) = Stack.pop s in
      if node = dest_id then (Hashtbl.add visited node pred; raise Stop);
      if not (Hashtbl.mem visited node) then begin
        try add_bindings node; Hashtbl.add visited node pred
        with Not_found -> () end;
      dfs () end in
    try dfs (); eprintf "Path not found@." with Stop ->
      eprintf "@[%a@]" (print_path visited src) dest

(*s Example of traversal between [Alan Turing]'s and [OCaml]'s pages. In order
  to experiment with different source/destination you need to patch this
  code. *)
  let my_bfs () = eprintf "Executing my Bfs... "; my_bfs "Alan_Turing" "Watson"
  let my_dfs () = eprintf "Executing my Dfs... "; my_dfs "Alan_Turing" "Watson"

(*s BFS function using [OCamlGraph] implementation. We use a first-class module
  to get the library's implementation: [let module Bfs = Traverse.Bfs(Links)
  in]. We call the function [Bfs.iter_component] to traverse the entire
  component of vertex [src_id]. This will cause all the vertices in the
  graph component of [src_id] to be reached. Once the [Bfs.iter_component]
  halts, we check if the [dest_id] has been marked as visited. Using the library
  provided functions, we are not able to recover path information, so we only
  answer the query "is there a path between [src] and [dest]?".

  Exercise: how can we interrupt the traversal once vertex [dest_id] is reached,
  so that we don't need to traverse the whole graph component of [src_id]? *)
  let ocamlgraph_bfs () =
    eprintf "Executing bfs from OCamlGraph... ";
    let module Bfs = Traverse.Bfs(Links) in
    let visited = Hashtbl.create (1 lsl 10) in
    let src_id = Hashtbl.find str_to_id_tbl "Alan_Turing" in
    let dest_id = Hashtbl.find str_to_id_tbl "Watson" in
    Bfs.iter_component (fun v -> Hashtbl.add visited v ()) links_graph src_id;
    eprintf "Done! ";
    if Hashtbl.mem visited dest_id then eprintf "Path found.@."
    else eprintf "No path found.@."

(*s DFS function using [OCamlGraph] implementation. We use a first-class module
  to get the library's implementation: [let module Dfs = Traverse.Dfs(Links)
  in]. We call the function [Dfs.prefix_component] to traverse the entire
  component of vertex [src_id]. This will cause all the vertices in the
  graph component of [src_id] to be reached. Once the [Dfs.iter_component]
  halts, we check if the [dest_id] has been marked as visited. Using the library
  provided functions, we are not able to recover path information, so we only
  answer the query "is there a path between [src] and [dest]?".

  Exercise: how can we interrupt the traversal once vertex [dest_id] is reached,
  so that we don't need to traverse the whole graph component of [src_id]?

  Exercise: why did we use the [prefix_component] function instead of the
  [postfix_component] one, also provided by [OCamlGraph]?
*)
  let ocamlgraph_dfs () =
    eprintf "Executing dfs from OCamlGraph... ";
    let module Dfs = Traverse.Dfs(Links) in
    let visited = Hashtbl.create (1 lsl 10) in
    let src_id = Hashtbl.find str_to_id_tbl "Alan_Turing" in
    let dest_id = Hashtbl.find str_to_id_tbl "Watson" in
    Dfs.prefix_component (fun v -> Hashtbl.add visited v ()) links_graph src_id;
    eprintf "Done! ";
    if Hashtbl.mem visited dest_id then eprintf "Path found.@."
    else eprintf "No path found.@."


(*
  Dijkstra algorithm from ocamlgraph
  *)
  module W = struct
    type label = int
    type t = int
    type edge = Links.E.t
    let weight _ = 1
    let zero = 0
    let add = (+)
    let compare = compare
  end

  module Dij = Path.Dijkstra(Links)(W)

let print_paath (v1,v2) = sprintf "%s -> %s \n" (Hashtbl.find id_to_str_tbl v1) (Hashtbl.find id_to_str_tbl v2)

  let ocamlgraph_dij () =
  eprintf "Executing Dijkstra from OCamlGraph... ";
  let module Dij = Path.Dijkstra(Links)(W) in
  let src_id = Hashtbl.find str_to_id_tbl "Alan_Turing" in
  let dest_id = Hashtbl.find str_to_id_tbl "Watson" in
  let (lo,liy) = Dij.shortest_path links_graph src_id dest_id in
  List.fold_left (fun acc i -> acc ^ print_paath (i)) "" lo




(*s Entry point for the scanner.*)
  let () =
    read_and_dump ();
    my_dfs (); my_bfs ();
    ocamlgraph_bfs (); ocamlgraph_dfs ();
    print_endline(ocamlgraph_dij ());
    eprintf "Vertex : %d " (Links.nb_vertex links_graph);
    eprintf "Edges  :%d @." (Links.nb_edges links_graph)

}

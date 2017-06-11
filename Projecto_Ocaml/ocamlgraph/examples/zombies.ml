(*
- Matriz quadrada n*n
- Existem : -> pessoas
            -> zombies
            -> gatos
- Regras :
          1) apenas os zombies podem infectar
          2) zombies podem afectar pessoas, mas não gatos
          3) zombies so podem afectar os vizinhos

          P -> pessoas
          # -> gatos
          Z -> zombies

          Tabuleiro inicial               Tabuleiro final

          | Z # P # P P P |              | Z # Z # P P P |
          | P P P P # P P |              | Z Z Z Z # P P |
          | # P # # # P P |              | # Z # # # P P |
          | P # P P P P P |   ------->   | P # P P P P P |
          | P P P P # P P |              | P P P P # P P |
          | P # P # P # # |              | P # P # Z # # |
          | P # P # Z P P |              | P # P # Z Z Z |


          Z#P#PPP
          PPPP#PP
          #P###PP
          P#PPPPP
          PPPP#PP
          P#P#P##
          P#P#ZPP


 - Abordagem com OcamlGraph:

 -> Grafo Regular não dirigido
 ->


*)

open Format
open Printf
open Graph

module G = Imperative.Graph.Abstract ( struct type t = int * int end )

let g = G.create ()

let nodes =
  let new_node i j =
    let v = G.V.create (i, j) in G.add_vertex g v ; v
  in
  Array.init 7 (fun i -> Array.init 7 (new_node i))

let node i j = nodes.(i).(j)

(* --------------display------------- *)

let display () =
  for i = 0 to 6 do
    for j = 0 to 6 do
      printf "%c" (Char.chr (G.Mark.get (node i j)))
    done;
    printf "\n";
  done;
  printf "\n@?"

(* --------------------------------- *)

let zombie_atack i j =
  match (Char.chr (G.Mark.get (node i j))) with
  | 'P' as ch -> G.Mark.set (node i j) (Char.code 'Z')
  | _ -> ()




(* --------------------------------- *)
let () =
    for i = 0 to 6 do
      let s = read_line () in
        for j = 0 to 6 do
        match s.[j] with
        | '#' as ch -> G.Mark.set (node i j) (Char.code ch )
        | 'P' as ch -> G.Mark.set (node i j) (Char.code ch )
        | 'Z' as ch -> G.Mark.set (node i j) (Char.code ch )
        | _ -> ()
        done
      done;;

      let () =
      for a = 0 to 5 do
        for i = 0 to 6 do for j = 0 to 6 do
          if i < 6 then(
            G.add_edge g (node i j) (node (i+1) j);
            if (Char.chr (G.Mark.get (node i j))) =  'Z' then
              zombie_atack (i+1) j
              );
          if j < 6 then(
            G.add_edge g (node i j) (node i (j+1));
            if (Char.chr (G.Mark.get (node i j))) =  'Z' then
              zombie_atack i (j+1)
              );
          if j > 0 then(
            G.add_edge g (node i j) (node i (j-1));
            if (Char.chr (G.Mark.get (node i j))) =  'Z' then
              zombie_atack i (j-1)
            );
            if i > 0 then(
              G.add_edge g (node i j) (node (i-1) j);
              if (Char.chr (G.Mark.get (node i j))) =  'Z' then
                zombie_atack (i-1) j
                );
          done done;
          done;




      display ();
      print_string ("\nVertices :");
      print_int (G.nb_vertex g);
          print_string ("\nArestas :");
      print_int (G.nb_edges g);
  printf "---------@.\n"
  ;;






(* ------------------------------------------------------

let faz =
  for i = 0 to 6 do
    for j = 0 to 6 do
    match (Char.chr (G.Mark.get (node i j))) with
    | 'Z' as ch ->
                  (
                   if j < 6 then
                    if i < 6 then(
                      if ((Char.chr (G.Mark.get (node (i) (j+1)))) = 'P') then (G.Mark.set (node i (j+1)) (Char.code ch));
                      if ((Char.chr (G.Mark.get (node (i) (j-1)))) = 'P') then (G.Mark.set (node i (j-1)) (Char.code ch));
                      if ((Char.chr (G.Mark.get (node (i+1) (j)))) = 'P') then (G.Mark.set (node (i+1) (j)) (Char.code ch));
                      if ((Char.chr (G.Mark.get (node (i-1) (j)))) = 'P') then (G.Mark.set (node (i-1) (j)) (Char.code ch));
                      )
                      )
    | _ ->  ()
    done
  done;;


  let faz_again =
  for i = 6 downto 1 do
    for j = 6 downto 1 do
    match (Char.chr (G.Mark.get (node i j))) with
    | 'Z' as ch ->
                  (
                   if j < 6 then
                    if i < 6 then(
                      if ((Char.chr (G.Mark.get (node (i) (j+1)))) = 'P') then (G.Mark.set (node i (j+1)) (Char.code ch));
                      if ((Char.chr (G.Mark.get (node (i) (j-1)))) = 'P') then (G.Mark.set (node i (j-1)) (Char.code ch));
                      if ((Char.chr (G.Mark.get (node (i+1) (j)))) = 'P') then (G.Mark.set (node (i+1) (j)) (Char.code ch));
                      if ((Char.chr (G.Mark.get (node (i-1) (j)))) = 'P') then (G.Mark.set (node (i-1) (j)) (Char.code ch));
                      )
                      )
    | _ ->  ()
    done
  done;;



display ();;

 module C = Coloring.Mark(G)

 let () = C.coloring g 9; display ()
 *)

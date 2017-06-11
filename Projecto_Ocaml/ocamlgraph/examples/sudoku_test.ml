open Graph


module G = Imperative.Graph.Abstract (struct type t = int * int end)

let g = G.create ()

let nodes =
  let new_node i j =
    let v = G.V.create (i, j) in G.add_vertex g v;
    v
  in
  Array.init 9 (fun i -> Array.init 9 (new_node i))

let node i j = nodes.(i).(j)

let () =
  for i = 0 to 8 do for j = 0 to 8 do
    for k = 0 to 8 do
      if k <> i then G.add_edge g (node i j) (node k j);
      if k <> j then G.add_edge g (node i j) (node i k);
    done;
    let gi = 3 * (i / 3) and gj = 3 * (j / 3) in
    for di = 0 to 2 do for dj = 0 to 2 do
      let i' = gi + di and j' = gj + dj in
      if i' <> i || j' <> j then
        G.add_edge g (node i j) (node i' j')
    done done
  done done

let () =
  for i = 0 to 8 do
    let s = read_line () in
    for j = 0 to 8 do
      match s.[j] with
      | '1'..'9' as ch -> G.Mark.set (node i j) (Char.code ch - Char.code '0')
      | _ -> ()
    done
  done

module C = Coloring.Mark(G)
let () = C.coloring g 9

let () =
  for i = 0 to 8 do
    for j = 0 to 8 do
      Format.printf "%d" (G.Mark.get (node i j))
    done;
    Format.printf "\n";
  done;
  Format.printf "@?"

open Core
open Tools

type pos = int * int  [@@deriving eq, hash, ord, sexp, show]
type vertex = 
| Place of pos * direction 
| End of pos
[@@deriving eq, hash, ord, sexp, show]

module Vertex = struct
  type t = vertex [@@deriving eq, hash, ord, sexp]

  let create end_pos pos direction =
    if equal_pos end_pos pos then End end_pos else Place (pos, direction)
  let update_direction update = function
  | Place (pos, direction) -> Place (pos, update direction)
  | End pos -> End pos
end

module G = Graph.Imperative.Digraph.Concrete(Vertex)

let next_vertices maze (end_pos: pos) (from_vertex: vertex): vertex list =
  let to_list = function
  | Place (pos, direction) ->
      let to_pos = add_direction pos direction in
      grid_at maze to_pos
      |> Option.filter ~f:(fun ch -> Char.(ch <> '#'))
      |> Option.value_map ~f:(fun _ -> [Vertex.create end_pos to_pos direction]) ~default:[]
  | _ -> [] in
  List.concat [
    to_list from_vertex;
    to_list @@ Vertex.update_direction turn_left from_vertex;
    to_list @@ Vertex.update_direction turn_right from_vertex;
  ]

let print_vertices = G.iter_vertex (show_vertex >> print_endline)

let mk_graph next_vertices start_vertex =
  let graph = G.create () in
  let visited = Hash_set.create (module Vertex) in
  let rec visit vertex = (
    if not (Hash_set.mem visited vertex) then (
      Hash_set.add visited vertex;
      G.add_vertex graph vertex;
      let next = next_vertices vertex in
      List.iter next ~f:(fun next_vertex ->
        G.add_edge graph vertex next_vertex;
        visit next_vertex
      )
    )
  ) in
  visit start_vertex;
  graph

let print_maze_with_path maze edges =
  let vertex_position = function
  | Place (pos, _) -> pos
  | End pos -> pos in
  let edge_positions = List.concat_map edges ~f:(fun (v1,v2) ->
    [vertex_position v1; vertex_position v2]
  ) |> IntTupleSet.of_list in
  let show_row y row = 
    Array.mapi row ~f:(fun x -> function
    | _ when Set.mem edge_positions (x,y) -> 'O'
    | ch -> ch
    )
    |> String.of_array in
  for y = 0 to (Array.length maze - 1) do
    print_endline @@ show_row y maze.(y);
  done
  
let part_a filename = 
  let maze = In_channel.read_lines filename
  |> List.map ~f:String.to_array  
  |> Array.of_list in
  let start_pos = find_in_grid maze ~f:(fun ch -> Char.(ch = 'S')) |> Option.value_exn in
  let start_vertex = Place (start_pos, E) in
  let end_pos = find_in_grid maze ~f:(fun ch -> Char.(ch = 'E')) |> Option.value_exn in
  let end_vertex = End end_pos in
  let next_vertices = next_vertices maze end_pos in
  let graph = mk_graph next_vertices start_vertex in
  let module Weight = struct
    type edge = G.E.t
    type t = int
    let weight ((v1,v2): edge) = 
      match (v1, v2) with
      | Place (p1,d1), Place (p2,d2) ->
        if not @@ equal_direction d1 d2 
          then 1001
          else if not @@ equal_pos p1 p2
            then 1
            else 0
      | Place (_,_), End _ -> 1
      | End _, Place (_,_) -> 1
      | End _, End _ -> 0
    let compare = Int.compare
    let add w1 w2 = w1 + w2
    let zero = 0
  end in
  let module Dijkstra = Graph.Path.Dijkstra(G)(Weight) in
  let shortest_path = Dijkstra.shortest_path graph start_vertex end_vertex in
  let (_, d) = shortest_path in
  d

let part_b _filename = 
  0

(* module Dijkstra (Elt : CCHeap.TOTAL_ORD) = struct
  module IntEltTuple = struct
    type t = int * Elt.t [@@deriving hash, ord]
  end
  module Heap = CCHeap.Make_from_compare(IntEltTuple)

  let shortest_path v1 v2 next_edges iter =
    let q = Heap.empty in
    let q = ref (Heap.add q (0, v1)) in
    let count_map = Hashtbl.create (module IntEltTuple) in
    let count v = Hashtbl.find count_map v |> Option.value ~default:0 in
    let inc v = Hashtbl.update count_map v ~f:(Option.value_map ~default:1 ~f:succ) in
    let loop () =
      if count v2 < k then 
        let (l, u) = Heap.pop_maximum q in 
        if count u < k then begin
          inc u;
          if u = v2 then
            iter l;
          next_edges u |> List.iter ~f:(fun u v weight ->
            q := Heap.add q (l + weight, v);
          ) in
          loop ();
        end in
    loop ()
end *)

(*
https://codeforces.com/blog/entry/102085

need priority heap with weight fn
try:
https://lewinb.net/posts/22_priorityqueue_ocaml/

q = empty min heap
count = 0-filled array
push (0, s) on q
while count[t] < k:
    (l, u) = pop q
    if count[u] == k:
        continue
    count[u] += 1
    if u == t:
        found a path of length l
    for each outgoing edge (u, v, w) from u:
        push (l + w, v) on q
        *)

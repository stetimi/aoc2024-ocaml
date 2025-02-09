open! Core
open Tools
open Timings

type pos = int * int [@@deriving eq, hash, ord, sexp, show]

type vertex = {
  pos: pos;
  direction: direction
} [@@deriving eq, hash, ord, sexp, show]

module Vertex = struct
  type t = vertex [@@deriving eq, hash, ord, sexp, show]
end
module VertexSet = Set.Make(Vertex)

let vertices maze =
  Array.concat_mapi maze ~f:(fun y row ->
    Array.mapi row ~f:(fun x ch ->
      if Char.(ch <> '#') 
        then List.map directions ~f:(fun direction -> {pos=(x,y); direction})
        else [] 
    )
  ) |> Array.to_list |> List.concat

let is_valid_direction_change d1 d2 = match d1,d2 with 
| (N,E) | (E,S) | (S,W) | (W,N) | (E,N) | (S,E) | (W,S) | (N,W) -> true
| (N,N) | (E,E) | (S,S) | (W,W) -> true
| _ -> false

let neighbours vertices_set {pos; direction} =
  List.filter_map directions ~f:(fun direction' ->
    let neighbour = if equal_direction direction direction'
      then {pos=add_direction pos direction; direction}
      else {pos; direction=direction'} in
    Option.some_if (Set.mem vertices_set neighbour && is_valid_direction_change direction direction') neighbour
  )

let weight {pos=p1; direction=d1} {pos=p2; direction=d2} = 
  let direction_score = if equal_direction d1 d2 then 0 else 1000 in
  let pos_score = if equal_pos p1 p2 then 0 else 1 in
  direction_score + pos_score

let end_dists end_pos dists =
  directions
  |> List.filter_map ~f:(fun direction -> Hashtbl.find dists {pos=end_pos; direction})

let run_dijkstra maze =
  let vertices = vertices maze in
  let source_pos = find_in_grid maze ~f:Char.(fun ch -> ch = 'S') |> Option.value_exn in
  let source = {pos=source_pos; direction=E} in
  let end_pos = find_in_grid maze ~f:Char.(fun ch -> ch = 'E') |> Option.value_exn in
  let vertices_set = VertexSet.of_list vertices in
  let neighbours = neighbours vertices_set in
  let is_end {pos;direction=_} = IntTuple.equal pos end_pos in
  let module Dijkstra = Search.Dijkstra(Vertex) in
  Dijkstra.all_shortest_paths
    ~vertices
    ~neighbours
    ~weight 
    ~source 
    ~is_end

type maze_info = {
  maze: char array array;
  end_pos: int * int
}

let read_maze filename =
  let maze = In_channel.read_lines filename
  |> List.map ~f:String.to_array  
  |> Array.of_list in
  let end_pos = find_in_grid maze ~f:Char.(fun ch -> ch = 'E') |> Option.value_exn in
  maze, end_pos

let part_a (maze, end_pos) = 
  let dists, _ = run_dijkstra maze in
  end_dists end_pos dists |> List.min_elt ~compare:Int.compare |> Option.value ~default:0

let part_b (maze, end_pos)  =
  let dists, prevs = run_dijkstra maze in
  let min_dist = end_dists end_pos dists |> List.min_elt ~compare:Int.compare |> Option.value ~default:0 in
  let end_vertex = Hashtbl.to_alist dists
  |> List.filter ~f:(fun ({pos;direction=_},dist) -> equal_pos pos end_pos && dist = min_dist)
  |> single_exn
  |> fst in
  let queue = Queue.singleton end_vertex in
  let on_path = Hash_set.create (module IntTuple) in
  let rec bfs () =
    match Queue.dequeue queue with
    | None -> ()
    | Some vertex ->
        let nexts = Hashtbl.find_exn prevs vertex in
        Set.iter nexts ~f:(fun next ->
          let {pos;direction=_} = next in
          Queue.enqueue queue next;
          Hash_set.add on_path pos;
        );
        bfs ()
  in 
  bfs ();
  1 + Hash_set.length on_path

let solve = solve read_maze part_a part_b

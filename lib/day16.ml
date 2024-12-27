open Core
open Tools

type maze = char array array
type pos = int * int
type vertex = pos * direction
type edge = vertex * vertex
type path = (vertex, edge) CCGraph.t

let read_maze (filename: string): maze =
  In_channel.read_lines filename
  |> List.map ~f:String.to_array  
  |> Array.of_list

let next_vertices maze (from_vertex: vertex): vertex list =
  let (from_pos, from_direction) = from_vertex in
  let to_list pos direction = (
    let to_pos = add_direction pos direction in
    grid_at maze to_pos
    |> Option.filter ~f:(fun ch -> Char.(ch = '.'))
    |> Option.value_map ~f:(fun _ -> [(to_pos, direction)]) ~default:[]
  ) in
  List.concat [
    to_list from_pos from_direction;
    to_list from_pos @@ turn_left from_direction;
    to_list from_pos @@ turn_right from_direction;
  ]

let part_a _filename = 
  (* let maze = read_maze filename in
  let _start = find_in_grid maze ~f:(fun ch -> Char.(ch = 'S')) in
  let _end = find_in_grid maze ~f:(fun ch -> Char.(ch = 'E')) in
  let graph_iter = mk_iter (next_vertices maze) in
  let graph = CCGraph.make graph_iter in
  let table: vertex CCGraph.set = failwith "" in (* CCGraph.mk_map ~cmp:Int.compare () in *)
  let dist: edge -> int = failwith " " in
  let _v_iter = CCGraph.Traverse.dijkstra ~graph ~tbl:table ~dist in *)
  0

let part_b _filename = 0
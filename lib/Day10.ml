open Core
open Tools

type grid = int array array
type pos = int * int
type direction = int * int

let add_pos (x1,y1) (x2,y2) = x1+x2,y1+y2

let is_in_grid (x,y) g = 
  x >= 0 && y >= 0 && x < Array.length g.(0) && y < Array.length g

let at (x,y) g = 
  if is_in_grid (x,y) g then (Some g.(y).(x)) else None

let read_grid (filename: string): grid =
  In_channel.read_lines filename
  |> List.map ~f:(fun row -> String.to_array row |> Array.map ~f:(fun ch -> Char.to_int ch - 48)) 
  |> List.to_array

let hash_set_filter_visited hash_set nexts = 
  let nexts = List.filter nexts ~f:(Fn.non @@ Hash_set.mem hash_set) in
  List.iter nexts ~f:(Hash_set.add hash_set);
  nexts

let find_trailheads (grid: grid): pos array =
  let filter_row y = Array.filter_mapi ~f:(fun x h -> Option.some_if (h = 0) (x, y)) in
  Array.concat_mapi grid ~f:filter_row

let directions = [0,-1; 1,0; 0,1; -1,0]

let next_steps_in_trail (grid: grid) (pos: pos): pos list =
  let curr = at pos grid |> Option.value_exn in
  directions
  |> List.filter_map ~f:(fun d ->
    let pos = add_pos pos d in
    at pos grid 
    |> Option.filter ~f:(fun height -> height = curr + 1)
    |> Option.map ~f:(Fn.const pos)
  )

let end_of_trail grid pos =
  Option.value_map (at pos grid) ~default:false ~f:(fun h -> h = 9)

let follow_trails trails_map =
  let trailheads = find_trailheads trails_map in
  Array.map trailheads ~f:(fun trailhead ->
    let next curr = next_steps_in_trail trails_map curr in
    dfs 
      ~init:trailhead 
      ~next
      ~is_target:(end_of_trail trails_map)
  ) 

let part_a filename = 
  let trails_map = read_grid filename in
  let trails = follow_trails trails_map in
  Array.sum (module Int) trails ~f:(fun xs -> Hash_set.of_list (module IntTuple) xs |> Hash_set.to_list |> List.length)

let part_b filename = 
  let trails_map = read_grid filename in
  let trails = follow_trails trails_map in
  Array.sum (module Int) trails ~f:List.length

open Core

type grid = int array array
type pos = int * int
type direction = int * int

let add_pos (x1,y1) (x2,y2) = x1+x2,y1+y2

let is_in_grid (x,y) g = 
  x >= 0 && y >= 0 && x < Array.length g.(0) && y < Array.length g

let at (x,y) g = 
  if is_in_grid (x,y) g then (Some g.(y).(x)) else None

module IntTuple = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) = 
    match Int.compare x0 x1 with
      0 -> Int.compare y0 y1
    | c -> c

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  let hash (x,y) = Int.shift_left (Int.hash x) 16 + Int.hash y
end

let read_grid (filename: string): grid =
  In_channel.read_lines filename
  |> List.map ~f:(fun row -> String.to_array row |> Array.map ~f:(fun ch -> Char.to_int ch - 48)) 
  |> List.to_array

let hash_set_filter_visited hash_set nexts = 
  let nexts = List.filter nexts ~f:(Fn.non @@ Hash_set.mem hash_set) in
  List.iter nexts ~f:(Hash_set.add hash_set);
  nexts

let dfs ~(filter_visited: 'a list -> 'a list) ~(init:'a) ~(next: 'a -> 'a list) ~(is_target:'a -> bool): 'a list =
  let items = Stack.create () in
  Stack.push items init;
  let rec go found = (
    match Stack.pop items with
    | None -> 
        found
    | Some curr -> 
        let nexts = next curr |> filter_visited in
        List.iter nexts ~f:(Stack.push items);
        let found = if is_target curr 
          then curr :: found
          else found in
        go found
  ) in
  go []

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
    let filter_visited = Fn.id in
    let next curr = next_steps_in_trail trails_map curr |> filter_visited in
    dfs 
      ~filter_visited
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

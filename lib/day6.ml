open! Core
open Timings

let read_map filename =
  In_channel.read_lines filename
  |> Grid.from_strings

type guard = {pos: int; direction: Grid.direction}
[@@deriving ord, sexp_of]

module Guard = struct 
  module T = struct
    type t = guard    
    let compare = compare_guard
    let sexp_of_t = sexp_of_guard
  end
  include T
  include Comparator.Make(T)
end

let rotate guard = {guard with direction = Grid.rotate_clockwise_90 guard.direction}

type ending =
| OffGrid 
| Looped

let rec walk1 guard grid =
  let pos = Grid.to_xy guard.pos grid in
  let (x', y') = Grid.move pos guard.direction in
  if Grid.is_in_grid (x', y') grid then
    let ch = Grid.at_exn (x', y') grid in
    if Char.(ch = '#') then walk1 (rotate guard) grid
    else Some {guard with pos = Grid.to_index x' y' grid}
  else None

let walk (start_pos: int) (start_direction: Grid.direction) grid =
  let start_guard = {pos=start_pos; direction=start_direction} in
  let visited = Set.singleton (module Guard) start_guard in
  let rec do_walk visited guard = (
    match walk1 guard grid with 
    | Some guard when not @@ Set.mem visited guard -> do_walk (Set.add visited guard) guard
    | Some _ -> (visited, Looped)
    | _  -> (visited, OffGrid)
  ) in
  do_walk visited start_guard

let walk_get_visited_positions start_pos start_direction grid =
  walk start_pos start_direction grid
  |> fst
  |> Set.map (module Int) ~f:(fun g -> g.pos)

let part_a grid = 
  let start_pos = Grid.find_index_exn ~f:(Char.equal '^') grid in
  let start_direction = Grid.North in
  walk_get_visited_positions start_pos start_direction grid
  |> Set.length

let part_b grid = 
  let start_pos = Grid.find_index_exn ~f:(Char.equal '^') grid in
  let start_direction = Grid.North in
  let possible_block_positions = walk_get_visited_positions start_pos start_direction grid 
  |> Set.filter ~f:(fun p -> Char.(Grid.at_exn (Grid.to_xy p grid) grid = '.')) in
  (* Optimize by noting the path of the guard before each placed obstacle.
     Do the walk from the end of that path instead of from the start each time.
  *)
  Set.count possible_block_positions ~f:(fun pos -> (
    let xy = Grid.to_xy pos grid in
    let ch = Grid.at_exn xy grid in
    Grid.set_exn xy '#' grid; 
    let (_, ending) = walk start_pos start_direction grid in
    Grid.set_exn xy ch grid;
    match ending with 
    | Looped -> true
    | _ -> false
  ))

let solve = solve read_map part_a part_b

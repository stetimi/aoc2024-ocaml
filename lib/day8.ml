open! Core
open Timings

let read_map filename =
  In_channel.read_lines filename 
  |> Grid.from_strings

let pairs_of xs =
  let make_pairs = (function
  | [] -> None
  | x :: xs -> Some (Sequence.map (Sequence.of_list xs) ~f:(Tuple2.create x), xs)
  ) in
  Sequence.unfold ~init:xs ~f:make_pairs |> Sequence.concat

let antinodes generate_antinodes grid p1 p2 =
  let (x1,y1) = Grid.to_xy p1 grid in
  let (x2,y2) = Grid.to_xy p2 grid in
  let dx = x2-x1 in
  let dy = y2-y1 in
  let (back_antinodes, front_antinodes) = generate_antinodes x1 y1 x2 y2 dx dy in
  let drop_out_of_grid = Sequence.take_while ~f:(fun xy -> Grid.is_in_grid xy grid) in
  let back_antinodes = drop_out_of_grid back_antinodes in
  let front_antinodes = drop_out_of_grid front_antinodes in
  Sequence.concat @@ Sequence.of_list [back_antinodes; front_antinodes]
  |> Sequence.map ~f:(fun (x, y) -> Grid.to_index x y grid)

let ints = Sequence.unfold ~init:0 ~f:(fun n -> Some (n,n+1))

let generate_resonant_antinode_pairs multiples x1 y1 x2 y2 dx dy =
  let back_antinodes = Sequence.map multiples ~f:(fun n -> (x1-n*dx,y1-n*dy)) in 
  let front_antinodes =Sequence.map multiples ~f:(fun n -> (x2+n*dx,y2+n*dy)) in 
  back_antinodes, front_antinodes

let add_antenna_position (pos, ch) m =
  Map.update m ch ~f:(Option.value_map ~f:(List.cons pos) ~default:[pos])

let antenna_positions (grid: char Grid.t) = 
  grid.cells
  |> List.of_array
  |> List.filter_mapi ~f:(fun i ch -> 
    if Char.(ch <> '.') then Some (i, ch) else None
  )
  |> List.fold_right ~init:(Map.empty (module Char)) ~f:add_antenna_position

let antinode_positions antinodes grid =
  let antenna_positions = antenna_positions grid in
  let antenna_pairs = Map.map antenna_positions ~f:pairs_of in
  let antinodes = Map.map antenna_pairs ~f:(Sequence.concat_map ~f:(fun (t1,t2) -> antinodes grid t1 t2)) in
  Map.to_alist antinodes 
  |> Sequence.of_list 
  |> Sequence.concat_map ~f:snd 
  |> Sequence.to_list 
  |> Set.of_list (module Int)

let part_a map = 
  map
  |> antinode_positions @@ antinodes (generate_resonant_antinode_pairs @@ Sequence.singleton 1)
  |> Set.length

let part_b map = 
  map
  |> antinode_positions @@ antinodes (generate_resonant_antinode_pairs ints)
  |> Set.length

let solve = solve read_map part_a part_b


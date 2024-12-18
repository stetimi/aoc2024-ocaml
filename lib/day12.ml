open Core
open Tools

type garden = char array array
let is_in_garden garden (x, y) = 
  let width = Array.length garden.(0) in
  let height = Array.length garden in
  x >= 0 && y >= 0 && x < width && y < height

let read_garden filename =
  In_channel.read_lines filename
  |> List.map ~f:String.to_array
  |> List.to_array

let garden () = read_garden "test/test_inputs/day12.txt"

let find_region garden pos =
  let seen = Hash_set.create (module IntTuple) in
  let is_in_garden = is_in_garden garden in
  Hash_set.add seen pos;
  let region_char = garden.(snd pos).(fst pos) in
  let next (x,y) = (
    [0,-1;1,0;0,1;-1,0]
    |> List.map ~f:(fun (dx, dy) -> (x+dx, y+dy))
    |> List.filter ~f:(fun (x,y) -> 
      is_in_garden (x,y) && Char.(garden.(y).(x) = region_char) && not (Hash_set.mem seen (x,y))
    )
  ) in
  let rec go pos = (
    let nexts = next pos in
    if List.is_empty nexts 
      then ()
      else 
        List.iter nexts ~f:(Hash_set.add seen);
        List.iter nexts ~f:go;
  ) in 
  go pos;
  seen

let part_a filename = 
  let _garden = read_garden filename in
  0

let part_b filename =
  let _garden = read_garden filename in
  0

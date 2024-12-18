open Core
open Tools

type garden = char array array

let positions garden =
  let width = Array.length (garden.(0)) in
  let height = Array.length garden in 
  List.cartesian_product (List.range 0 width) (List.range 0 height)

let is_in_garden garden (x, y) = 
  let width = Array.length garden.(0) in
  let height = Array.length garden in
  x >= 0 && y >= 0 && x < width && y < height

let read_garden filename =
  In_channel.read_lines filename
  |> List.map ~f:String.to_array
  |> List.to_array

let find_region garden (pos: IntTuple.t) =
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

let find_all_regions garden =
  let seen = Hash_set.create (module IntTuple) in
  let rec go acc  = (function
    | [] -> acc
    | pos :: rest ->
        let acc = if Hash_set.mem seen pos
          then acc
          else (
            let region = find_region garden pos in
            Hash_set.iter region ~f:(Hash_set.add seen);
            region :: acc
          ) in go acc rest
  ) in
  go [] (positions garden) |> List.map ~f:(IntTupleSet.of_hash_set)

let surroundings (x, y) = [x,y-1;x-1,y;x+1,y;x,y+1]

let perimeter (region: IntTupleSet.t): int =
  let perimeter points = List.count points ~f:(Fn.non @@ Set.mem region) in
  region
  |> Set.to_list
  |> List.sum (module Int) ~f:(surroundings >> perimeter)

let corners (region: IntTupleSet.t): int =
  let in_region = Set.mem region in
  let not_in_region = Fn.non in_region in
  let count_corners (x, y) = (
    let north = x, y - 1 in
    let east = x + 1, y in
    let south = x, y + 1 in
    let west = x - 1, y in
    let north_west = x - 1, y - 1 in
    let north_east = x + 1, y - 1 in
    let south_west = x - 1, y + 1 in
    let south_east = x + 1, y + 1 in
    let external_corner_count out1 out2 = (
      if not_in_region out1 && not_in_region out2 then 1 else 0) in
    let internal_corner_count out1 in1 in2 =
      if not_in_region out1 && in_region in1 && in_region in2 then 1 else 0 in
    (external_corner_count north west) + 
    (external_corner_count west south) + 
    (external_corner_count south east) + 
    (external_corner_count east north) +
    (internal_corner_count north_east north east) +
    (internal_corner_count north_west north west) +
    (internal_corner_count south_west south west) +
    (internal_corner_count south_east south east)
  ) in
  region
  |> Set.to_list
  |> List.sum (module Int) ~f:count_corners

let run count filename =
  let garden = read_garden filename in
  let regions = find_all_regions garden in
  let score region =
    let area = Set.length region in
    let count = count region in
    area * count in
  List.sum (module Int) regions ~f:score

let part_a = run perimeter

let part_b = run corners
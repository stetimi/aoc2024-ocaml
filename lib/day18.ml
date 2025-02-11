open Core
open Tools

let parse_coord s =
  let [@warning "-8"] [x; y] = String.split_on_chars s ~on:[',']
  |> List.map ~f:Int.of_string in
  x, y

let corrupt_coords filename =
  In_channel.read_lines filename
  |> List.map ~f:parse_coord

let path_length size (corrupt_coords: IntTupleSet.t) =
  let next pos =
    surroundings (size, size) pos
    |> List.filter ~f:(Fn.non @@ Set.mem corrupt_coords) in
  let next = track_seen ~f:Fn.id (Hash_set.create (module IntTuple)) next in
  let path = bfs
    ~init: (0,0)
    ~next
    ~is_target:(fun (x,y) -> x = size - 1 && y = size - 1)
  in
  List.length path

let part_a filename =
  let corrupt_coords = corrupt_coords filename in
  let first_1024 = List.take corrupt_coords 1024 |> IntTupleSet.of_list in
  path_length 71 first_1024

let part_b_search size start filename =
  let corrupt_coords = corrupt_coords filename in
  let rec go l = (
    let corrupt_coords = List.take corrupt_coords l in
    if path_length size (corrupt_coords |> IntTupleSet.of_list) = 0
      then List.last_exn corrupt_coords else go (succ l)
  ) in
  let x, y = go start in
  [%string "%{x#Int},%{y#Int}"]

let part_b = part_b_search 71 1025

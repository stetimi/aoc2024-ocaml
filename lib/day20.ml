open Core
open Tools

type track = (int * int) array

let read_track (filename: string): track =
  let grid = In_channel.read_lines filename
  |> List.map ~f:String.to_array
  |> List.to_array in
  let start = find_in_grid grid ~f:(fun ch -> Char.(ch = 'S')) |> Option.value_exn  in
  let width, height = dimensions grid in
  let can_move_to (x,y) =
    let cell = grid.(y).(x) in
    Char.(cell = '.' || cell = 'E') in
  let seen = Hash_set.create (module IntTuple) in
  Hash_set.add seen start;
  let rec go pos acc = (
    let x,y = pos in
    if Char.(grid.(y).(x) = 'E')
      then acc
      else 
        let next = surroundings (width, height) pos 
        |> List.filter ~f:(fun pos -> can_move_to pos && not @@ Hash_set.mem seen pos)
        |> single_exn in
        Hash_set.add seen next;
        go next (next::acc)
  ) in
  go start [start] |> List.rev |> List.to_array

let cheats max_dist (track: track) =
  let track_len = Array.length track in
  let indexed_track = zip_arrays_len track_len (array_range 0 track_len) track in
  Array.concat_map indexed_track ~f:(fun (t1, p1) ->
    array_range (t1) track_len |> Array.filter_map ~f:(fun t2 ->
      let dist = manhattan_dist p1 track.(t2) in
      Option.some_if (dist <= max_dist && t2 - t1 > dist) (t2 - t1 - dist)
    )
  )

let run max_dist min_cheat filename =
  let track = read_track filename in
  let cheats = cheats max_dist track in
  Array.count cheats ~f:(fun l -> l >= min_cheat)

let part_a = run 2

let part_b = run 20
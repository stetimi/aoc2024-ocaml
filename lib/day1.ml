open! Core
open Timings 

let parse_ints s =
  let to_int start finish = String.slice s start finish |> Int.of_string in
  [to_int 0 5; to_int 8 13]

let read_lists lines =
  let [@warning "-8"] [first; second] = List.map lines ~f:parse_ints |> List.transpose_exn in
  (first, second)

let read_input filename = In_channel.read_lines filename |> read_lists

let part_a (first, second) =
  let first, second = Tuple2.map (first, second) ~f:(List.sort ~compare:Int.compare) in
  let diffs = List.map2_exn first second ~f:(fun x y -> Int.abs (x - y)) in
  List.sum (module Int) diffs ~f:Fun.id

let part_b (first, second) =
  let module IntMultiSet = CCMultiSet.Make(Int) in
  let second = IntMultiSet.of_list second in
  List.sum (module Int) first ~f:(fun x -> x * IntMultiSet.count second x)

let solve = solve read_input part_a part_b
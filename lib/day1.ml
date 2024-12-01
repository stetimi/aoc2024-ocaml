open Core

let parse_ints s =
  let parts = String.split_on_chars s ~on:[' '] |> List.filter ~f:(fun s -> String.is_empty s |> not) in
  let x = List.hd_exn parts in
  let y = List.nth_exn parts 1 in
  [Int.of_string x; Int.of_string y]

let read_lists lines =
  let [@warning "-8"] [first; second] = List.map lines ~f:parse_ints |> List.transpose_exn in
  (first, second)

let day1A () =
  let lines = In_channel.read_lines "inputs/day1.txt" in
  let (first, second) = read_lists lines in
  let sort = List.sort ~compare:Int.compare in
  let diffs = List.zip_exn (sort first) (sort second) |> List.map ~f:(fun (x, y) -> Int.abs (x - y)) in
  List.sum (module Int) diffs ~f:(fun x -> x)
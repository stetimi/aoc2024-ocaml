open Core

let sum_muls: string -> int = fun text ->
  let re = Re2.create_exn {|mul\(([0-9]+),([0-9]+)\)|} in
  let matches = Re2.get_matches re text in
  let accumulate_matches total m = (
    let first = Re2.Match.get_exn m ~sub:(`Index 1) |> Int.of_string_opt in
    let second = Re2.Match.get_exn m ~sub:(`Index 2) |> Int.of_string_opt in
    match (first, second) with
    | (Some first, Some second) -> total + first * second
    | _ -> total) in
  match matches with
  | Error _ -> 0
  | Ok matches -> List.fold matches ~init:0 ~f:accumulate_matches

let part_a filename =
  In_channel.read_all filename
  |> sum_muls

let part_b _filename = 0
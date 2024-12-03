open Core

let (>>) f g x = g (f x)

let read_reports: string -> int list =
  String.split_on_chars ~on:[' '] 
  >> List.filter ~f:(String.is_empty |> Fun.negate)
  >> List.map ~f:(Int.of_string)


let read_input (): int list list = 
  In_channel.read_lines "inputs/day2.txt" |> List.map ~f:read_reports


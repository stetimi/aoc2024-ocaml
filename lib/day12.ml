open Core

let read_garden filename =
  In_channel.read_lines filename
  |> List.map ~f:String.to_array
  |> List.to_array

let part_a filename = 
  let _garden = read_garden filename in
  0

let part_b filename =
  let _garden = read_garden filename in
  0

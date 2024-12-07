open Core

let read_grid (filename: string): char Grid.t =
  let cells = In_channel.read_lines filename
  |> List.map ~f:(String.to_array) in
  let cols = List.hd_exn cells |> Array.length in
  Grid.init cols (Array.concat cells)

let re_xmas = Re.(str "XMAS" |> compile) 

let re_samx = Re.(str "SAMX" |> compile) 

let count_xmas_matches (cs: char array): int = 
  let s = String.of_array cs in
  let count re =  Re.all re s |> List.length in
  count re_xmas + count re_samx

let xmas_matches_at_point (g: char Grid.t) (xy: int * int): int = 
  let count delta = (
    let points = Grid.points xy delta 4 in
    let word = Grid.gather points '*' g |> String.of_array in
    if String.equal word "XMAS" then 1 else 0) in
  Grid.directions |> List.sum (module Int) ~f:count

let part_a filename = 
  let grid = read_grid filename in
  Grid.sum ~f:(xmas_matches_at_point grid) grid
  
let part_b _filename = 0
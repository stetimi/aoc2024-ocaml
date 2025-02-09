open! Core
open Timings

let read_grid (filename: string): char Grid.t =
  let cells = In_channel.read_lines filename
  |> List.map ~f:(String.to_array) in
  let cols = List.hd_exn cells |> Array.length in
  Grid.init cols (Array.concat cells)

let count_xmas_matches_at (g: char Grid.t) (xy: int * int): int = 
  let xmas = "XMAS" |> String.to_array in
  let matches delta = (
    let points = Grid.points xy delta 4 in
    let word = Grid.gather points '*' g in
    Array.equal Char.equal word xmas) in
  Grid.directions |> List.count ~f:matches

let cross_mas_matches_at (g: char Grid.t) (xy: int * int): bool = 
  let xmases = ["SSAMM"; "MSAMS"; "SMASM"; "MMASS"] |> Set.of_list (module String) in
  let offsets = [|(-1, -1); (1, -1); (0, 0); (-1, 1); (1, 1)|] in
  let points = Grid.apply_offsets xy offsets in
  let word = Grid.gather points '*' g |> String.of_array in
  Set.mem xmases word
  
let part_a grid = 
  Grid.sum ~f:(count_xmas_matches_at grid) grid
  
let part_b grid = 
  Grid.count ~f:(cross_mas_matches_at grid) grid

let solve = solve read_grid part_a part_b
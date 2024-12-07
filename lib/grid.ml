open Core

type 'a t = {cols: int; cells: 'a array}

let init (cols: int) (cells: 'a array): 'a t = {cols; cells}

let rows (grid: _ t) = Array.length grid.cells / grid.cols

let to_index (x: int) (y: int) (grid: _ t): int = x + y * grid.cols

let to_xy (p: int) (grid: _ t): (int * int) = 
  (p % grid.cols, p / grid.cols)

let to_dxdy (p: int) (d: int) (grid: _ t): (int * int) = 
  let (x, y) = to_xy p grid in
  let (x_end, y_end) = to_xy (p+d) grid in
  (x_end-x, y_end-y)
  
let max_line_length d start edge =
  if d = 0 then Int.max_value
  else if d > 0 then (edge - start / d)
  else (start / -d) - 1

let line_from (p: int) (delta: int) (grid: 'a t): 'a array =
  let (x, y) = to_xy p grid in
  let (dx, dy) = to_dxdy p delta grid in
  let edge_x = if dx < 0 then -1 else grid.cols in
  let edge_y = if dy < 0 then -1 else rows grid in 
  let x_len = if dx = 0 then Int.max_value else (edge_x - x) / dx in
  let y_len = if dy = 0 then Int.max_value else (edge_y - y) / dy in
  let length = min x_len y_len in
  Array.init length ~f:(fun j -> grid.cells.(p+delta*j))

let all_rows (g: 'a t): 'a array array =
  Array.init (rows g) ~f:(fun i -> 
    let p = i * g.cols in
    line_from p 1 g  
  )  

let all_cols (g: 'a t): 'a array array =
  Array.init g.cols ~f:(fun p -> line_from p g.cols g)
  
let all_tl_br_diagonals (g: 'a t): 'a array array =
  let top_row = List.range 1 g.cols in
  let left_col = List.init (rows g) ~f:(fun i -> i * g.cols) in
  let starts = List.concat [top_row; left_col] |> Array.of_list in
  Array.map starts ~f:(fun start -> line_from start (g.cols + 1) g)
    
let all_tr_bl_diagonals (g: 'a t): 'a array array =
  let top_row = List.range 0 (g.cols - 1) in
  let right_col = List.init (rows g) ~f:(fun i -> g.cols - 1 + i * g.cols) in
  let starts = List.concat [top_row; right_col] |> Array.of_list in
  Array.map starts ~f:(fun start -> line_from start (g.cols - 1) g)
  
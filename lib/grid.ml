open Core

type 'a t = {cols: int; cells: 'a array}

let init (cols: int) (cells: 'a array): 'a t = {cols; cells}

let rows (grid: _ t) = Array.length grid.cells / grid.cols

let to_index (x: int) (y: int) (grid: _ t): int = x + y * grid.cols

let directions = [
  (-1, -1); ( 0, -1); ( 1, -1);
  (-1,  0);           ( 1,  0);
  (-1,  1); ( 0,  1); ( 1,  1)
]

let gather (points: (int * int) array) (default: 'a) (grid: 'a t): 'a array =
  let at_1 (x, y) =
    if x >= 0 && x < grid.cols && y >= 0 && y < rows grid
      then grid.cells.(to_index x y grid)
      else default in
  Array.map points ~f:at_1

let points (x, y) (dx, dy) (len: int): (int * int) array =
  Array.init len ~f:(fun i -> (x + dx * i, y + dy * i))

let apply_offsets (x, y) (offsets: (int * int) array): (int * int) array =
  Array.map offsets ~f:(fun (dx, dy) -> (x+dx, y+dy))

let to_xy (p: int) (grid: _ t): (int * int) = 
  (p % grid.cols, p / grid.cols)

let sum ~(f: (int * int) -> int) (g: 'a t): int =
  let len = Array.length g.cells in
  let ps = Sequence.range 0 len in
  Sequence.sum (module Int) ps ~f:(fun p -> f (to_xy p g))

let count ~(f: (int * int) -> bool) (g: 'a t): int =
  let len = Array.length g.cells in
  let ps = Sequence.range 0 len in
  Sequence.count ps ~f:(fun p -> f (to_xy p g))
  
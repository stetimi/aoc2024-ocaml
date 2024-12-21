open Core

type cell =
| Space
| BoxLeft
| BoxRight
| Robot
| Wall
[@@deriving ord, show]

let cell_eq c1 c2 = compare_cell c1 c2 = 0

type warehouse = cell array array

type instruction = | Left | Right | Up | Down
[@@deriving ord, show]

type pos = int * int

let read_warehouse (lines: string list): warehouse =
  let to_row s = (
    String.to_array s |> Array.map ~f:(function
    | '.' -> Space
    | '#' -> Wall
    | '@' -> Robot
    | 'O' -> BoxLeft
    | ch -> failwith [%string "Bug: %{ch#Char}"]
  )) in
  lines
  |> List.map ~f:to_row
  |> List.to_array

let read_doubled_warehouse (lines: string list): warehouse =
  let to_row s = (
    String.to_list s |> List.map ~f:(function
    | '.' -> [Space; Space]
    | '#' -> [Wall; Wall]
    | '@' -> [Robot; Space]
    | 'O' -> [BoxLeft; BoxRight]
    | ch -> failwith [%string "Bug: %{ch#Char}"]
  )) in
  lines
  |> List.concat_map ~f:to_row
  |> List.map ~f:List.to_array
  |> List.to_array

let read_instructions (lines: string list): instruction list =
  let to_instructions s = (
    String.to_array s |> Array.map ~f:(function
    | '^' -> Up
    | '<' -> Left
    | '>' -> Right
    | 'v' -> Down
    | ch -> failwith [%string "Bug: %{ch#Char}"]
  ) |> Array.to_list) in
  List.concat_map lines ~f:to_instructions

let read_input filename =
  let  [@warning "-8"] (warehouse, _ :: instructions) = In_channel.read_lines filename
  |> List.split_while ~f:(Fn.non String.is_empty) in
  warehouse, instructions

let find_cells warehouse (mover: cell): (int * int) array =
  let find_x y row = Array.filter_mapi row ~f:(fun x cell -> if cell_eq cell mover then Some (x, y) else None) in
  Array.concat_mapi warehouse ~f:find_x
    
let find_robot warehouse =
  find_cells warehouse Robot |> Fn.flip Array.get 0

let rec move warehouse (mover: cell) (rx, ry) (instruction: instruction): pos = 
  let (dx, dy) = match  instruction with
  | Up -> 0 , -1 | Down -> 0, 1 | Left -> -1, 0 | Right -> 1, 0 in
  let (rx', ry') = rx+dx, ry+dy in
  match warehouse.(ry').(rx') with
  | Wall -> (rx, ry)
  | Robot -> failwith "found robot!"
  | Space -> 
      warehouse.(ry').(rx') <- mover;
      warehouse.(ry).(rx) <- Space;
      (rx', ry')
  | BoxLeft | BoxRight as cell  ->
      let (boxx', boxy') = move warehouse cell (rx',ry') instruction in
      if boxx' = rx' && boxy' = ry' 
        then (rx, ry)
        else (  
          warehouse.(ry').(rx') <- mover;
          warehouse.(ry).(rx) <- Space;
          (rx', ry')
        )
    
let part_a filename = 
  let (warehouse, instructions) = read_input filename |> Tuple2.map_both ~f1:read_warehouse ~f2:read_instructions in
  let roboti = find_robot warehouse in
  let _ = List.fold_left instructions ~init:roboti ~f:(move warehouse Robot) in
  let boxes = find_cells warehouse BoxLeft in
  Array.sum (module Int) boxes ~f:(fun (x, y) -> 100 * y + x)

let part_b filename = 
  let (warehouse, instructions) = read_input filename |> Tuple2.map_both ~f1:read_doubled_warehouse ~f2:read_instructions in
  let midpoint = Array.length warehouse.(0) / 2 in
  let roboti = find_robot warehouse in
  let _ = List.fold_left instructions ~init:roboti ~f:(move warehouse Robot) in
  let box_lefts = find_cells warehouse BoxLeft
  |> Array.filter ~f:(fun (x, _) -> x < midpoint) in
  let box_rights = find_cells warehouse BoxLeft 
  |> Array.filter ~f:(fun (x, _) -> x >= midpoint) in
  let boxes = Array.concat [box_lefts; box_rights] in
  Array.sum (module Int) boxes ~f:(fun (x, y) -> 100 * y + x)

open Core
open Tools

type cell =
| Space
| BoxLeft
| BoxRight
| Robot
| Wall
[@@deriving ord, show]

let cell_eq c1 c2 = compare_cell c1 c2 = 0

type warehouse = cell array array

let show_warehouse w = show_grid w ~f:(function
| Space -> '.'
| BoxLeft -> '['
| BoxRight -> ']'
| Robot -> '@'
| Wall -> '#'
)

type instruction = | Left | Right | Up | Down
[@@deriving ord, show]

type pos = int * int

let follow (x,y) = function
| Left -> x-1,y
| Right -> x+1,y
| Up -> x,y-1
| Down -> x,y+1

let follow_back (x,y) = function
| Left -> x+1,y
| Right -> x-1,y
| Up -> x,y+1
| Down -> x,y-1

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
    String.to_list s |> List.concat_map ~f:(function
    | '.' -> [Space; Space]
    | '#' -> [Wall; Wall]
    | '@' -> [Robot; Space]
    | 'O' -> [BoxLeft; BoxRight]
    | ch -> failwith [%string "Bug: %{ch#Char}"]
  )) in
  lines
  |> List.map ~f:to_row
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
  
let find_cells warehouse (cell: cell): (int * int) array =
  let find_x y row = Array.filter_mapi row ~f:(fun x c -> if cell_eq c cell then Some (x, y) else None) in
  Array.concat_mapi warehouse ~f:find_x
    
let find_robot warehouse =
  find_cells warehouse Robot |> Fn.flip Array.get 0

let rec move warehouse (mover: cell) (rx, ry) (instruction: instruction): pos = 
  let (rx', ry') = follow (rx,ry) instruction in
  match warehouse.(ry').(rx') with
  | Wall -> (rx, ry)
  | Robot -> failwith "found robot!"
  | Space -> 
      warehouse.(ry').(rx') <- mover;
      warehouse.(ry).(rx) <- Space;
      (rx', ry')
  | BoxLeft | BoxRight ->
      let (boxx', boxy') = move warehouse BoxLeft (rx',ry') instruction in
      if boxx' = rx' && boxy' = ry' 
        then (rx, ry)
        else (  
          warehouse.(ry').(rx') <- mover;
          warehouse.(ry).(rx) <- Space;
          (rx', ry')
        )

let find_boxes warehouse (instruction: instruction) (pos: pos): (cell * pos) list =
  let pos = follow pos instruction in 
  let seen = Hash_set.create (module IntTuple) in
  let rec go acc (x,y): (cell * pos) list = (
    if Hash_set.mem seen (x,y) then acc
    else (
      Hash_set.add seen (x,y);
      let cell = warehouse.(y).(x) in
      match cell with 
      | Space | Wall -> acc
      | Robot -> go acc @@ follow (x,y) instruction
      | BoxLeft -> 
          let acc = go acc (x+1,y) in
          let next = follow (x,y) instruction in
          go ((BoxLeft,(x,y)) :: acc) next
      | BoxRight -> 
          let acc = go acc (x-1,y) in
          let next = follow (x,y) instruction in
          go ((BoxRight,(x,y)) :: acc) next
    )
  ) in
  let sort_fn = match instruction with  
  | Left -> IntTuple.compare_x_first 
  | Right -> IntTuple.compare_x_last
  | Up -> IntTuple.compare_y_first 
  | Down -> IntTuple.compare_y_last in
  go [] pos |> List.sort ~compare:(fun (_,t1) (_,t2) -> sort_fn t1 t2)

let can_move warehouse (instruction: instruction) (boxes: pos list): bool =
  let nexts = List.map boxes ~f:(Fn.flip follow instruction) in
  List.for_all nexts ~f:(fun (x,y) -> not @@ cell_eq Wall warehouse.(y).(x))

let move_boxes warehouse (instruction: instruction) (boxes: (cell * pos) list) =
  List.iter boxes ~f:(fun (cell, (x, y)) -> 
    let x',y' = follow (x,y) instruction in
    warehouse.(y').(x') <- cell;
    warehouse.(y).(x) <- Space;
  )

let move_part_b warehouse (rx, ry) (instruction: instruction): pos =
  let boxes = find_boxes warehouse instruction (rx,ry) in
  let box_positions = List.map boxes ~f:snd in
  let rx', ry' = follow (rx,ry) instruction in
  if not @@ cell_eq Wall warehouse.(ry').(rx') && can_move warehouse instruction box_positions 
    then (
      move_boxes warehouse instruction boxes;
      warehouse.(ry).(rx) <- Space;
      warehouse.(ry').(rx') <- Robot;
      rx', ry'
    ) else (rx, ry)

let part_a filename = 
  let (warehouse, instructions) = read_input filename |> Tuple2.map_both ~f1:read_warehouse ~f2:read_instructions in
  let roboti = find_robot warehouse in
  List.fold_left instructions ~init:roboti ~f:(move warehouse Robot) |> Fn.const ();
  let boxes = find_cells warehouse BoxLeft in
  Array.sum (module Int) boxes ~f:(fun (x, y) -> 100 * y + x)

let part_b (filename): int = 
  let (warehouse, instructions) = read_input filename |> Tuple2.map_both ~f1:read_doubled_warehouse ~f2:read_instructions in
  let roboti = find_robot warehouse in
  List.fold_left instructions ~init:roboti ~f:(move_part_b warehouse) |> Fn.const ();
  let box_lefts = find_cells warehouse BoxLeft in
  let gps_coords = Array.map box_lefts ~f:(fun (x, y) -> 100 * y + x) in
  Array.sum (module Int) gps_coords ~f:Fn.id
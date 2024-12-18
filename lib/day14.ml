open Core

type robot = {
  mutable position: int * int;
  velocity: int * int;
}
[@@deriving ord, show]

let read_robot (line: string) =
  let [@warning "-8"] [pos_str; vel_str] =  line
  |> String.substr_replace_first ~pattern:"p=" ~with_:""
  |> String.substr_replace_first  ~pattern:"v=" ~with_:""
  |> String.split ~on:' ' in
  let read_tuple s = (
    let [@warning "-8"] [x; y] = String.split s ~on:',' 
    |> List.map ~f:Int.of_string in  
    x, y
  ) in
  {position=read_tuple pos_str; velocity=read_tuple vel_str}

let read_robots filename = 
  In_channel.read_lines filename 
  |> List.map ~f:read_robot

let move_robot (bounds_x, bounds_y) (count: int) (robot: robot): unit =
  let rem n p = (
    let rem = Int.rem n p in if rem < 0 then rem + p else rem
  ) in
  let move_1d bound sel (xy: int * int) dxy = rem ((sel xy) + (sel dxy) * count) bound in 
  let x = move_1d bounds_x fst robot.position robot.velocity in
  let y = move_1d bounds_y snd robot.position robot.velocity in
  robot.position <- x, y

let safety_factor (bounds_x, bounds_y) (robots: robot list) =
  let x_mid = bounds_x / 2 in
  let y_mid = bounds_y / 2 in
  let is_in_nw_quadrant (x, y) = x < x_mid && y < y_mid in
  let is_in_ne_quadrant (x, y) = x > x_mid && y < y_mid in
  let is_in_sw_quadrant (x, y) = x < x_mid && y > y_mid in
  let is_in_se_quadrant (x, y) = x > x_mid && y > y_mid in
  let scores = [|0;0;0;0;0|] in
  List.iter robots ~f:(fun robot -> 
    let index = match robot.position with
    | p when is_in_nw_quadrant p -> 0
    | p when is_in_ne_quadrant p -> 1
    | p when is_in_sw_quadrant p -> 2
    | p when is_in_se_quadrant p -> 3
    | _ -> 4 in
    scores.(index) <- scores.(index) + 1
  );
  scores.(4) <- 1;
  Array.fold_right scores ~init:1 ~f:( * )

let has_a_line robots_by_position robots =
  List.exists robots ~f:(fun robot ->
    let x, y = robot.position in
    List.for_all [1;2;3;4;5] ~f:(fun dy -> Char.(robots_by_position x (y + dy) <> '.'))
  )

let robots_by_position to_int (robots): (int -> int -> char) = 
  let positions = List.map robots ~f:(fun r -> to_int r.position) in
  let cell_display n = if n > 10 then '*' else Char.of_int_exn (n+48) in
  let m = List.fold_left positions ~init:(Map.empty (module Int)) ~f:(Map.update ~f:(Option.value_map ~default:1 ~f:Int.succ)) in
  let get x y = Map.find m (to_int (x, y)) |> Option.value_map ~f:cell_display ~default:'.' in
  get

let show_grid (bounds_x, bounds_y) (robots: robot list) =
  let to_int (x, y) = y * bounds_x + x in
  let get_cell = robots_by_position to_int robots in
  let show_row y = Array.init bounds_x ~f:(fun x -> get_cell x y) |> String.of_array in
  for y = 0 to bounds_y do
    print_endline @@ show_row y
  done

let part_a bounds filename = 
  let robots = read_robots filename in
  List.iter robots ~f:(move_robot bounds 100);
  safety_factor bounds robots 

let part_b bounds n filename =
  let bounds_x, _ = bounds in
  let to_int (x, y) = y * bounds_x + x in
  let robots = read_robots filename in
  for i = 1 to n do
    List.iter robots ~f:(move_robot bounds 1);
    let robots_by_position = robots_by_position to_int robots in
    if has_a_line robots_by_position robots then (
      print_endline [%string "Maybe %{i#Int}"];
      show_grid bounds robots;
      (* raise Exit *)
    )
  done;
  0

(* Run in utop, visually look for tree *)
let run n = part_b (101,103) n "inputs/day14.txt"
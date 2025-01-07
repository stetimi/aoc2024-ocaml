open Core
open Tools

let read_codes = In_channel.read_lines >> List.map ~f:String.to_list 

type keypad = (char, (int * int), Char.comparator_witness) Map_intf.Map.t

let numeric_keypad_alist = [
  'A',(0,0);
  '0',(-1,0);
  '3',(0,-1);
  '2',(-1,-1);
  '1',(-2,-1);
  '6',(0,-2);
  '5',(-1,-2);
  '4',(-2,-2);
  '9',(0,-3);
  '8',(-1,-3);
  '7',(-2,-3);
] 

let numeric_keypad = Map.of_alist_exn (module Char) numeric_keypad_alist
let reverse_numeric_keypad = List.map numeric_keypad_alist ~f:(fun (a,b) -> b,a) |> IntTupleMap.of_alist_exn

let directional_keypad_alist = [
  'A',(0,0);
  '^',(-1,0);
  '<',(-2,1);
  'v',(-1,1);
  '>',(0,1);
] 

let directional_keypad = Map.of_alist_exn (module Char) directional_keypad_alist
let reverse_directional_keypad = List.map directional_keypad_alist ~f:(fun (a,b) -> b,a) |> IntTupleMap.of_alist_exn

type path = char list
type shortest_path_fn = char -> char -> path
type shortest_paths_fn = char -> char -> path list

let sign n = Sign.to_int @@ Int.sign n

let rev_direction = function 
  | '^' -> 'v'
  | 'v' -> '^'
  | '<' -> '>'
  | '>' -> '<'
  | 'A' -> 'A'
  | _ -> failwith "Not a direction"

let to_direction pos = match pos with
| -1,0 -> '<'
| 1,0 -> '>'
| 0,-1 -> '^'
| 0,1 -> 'v'
| _ -> failwith [%string "Not a position representing a direction: (%{fst pos#Int},%{snd pos#Int})"]

let rec combinations n m n_elt m_elt =
  if n = 0 then [List.init m ~f:(Fn.const m_elt)]
  else if m = 0 then [List.init n ~f:(Fn.const n_elt)]
  else
    let with_m = List.map (combinations (n - 1) m n_elt m_elt) ~f:(fun l -> n_elt :: l) in
    let with_n = List.map (combinations n (m - 1) n_elt m_elt) ~f:(fun l -> m_elt :: l) in
    with_m @ with_n

let numeric_keypad_shortest_paths (src) (dest): path list =
  assert (let test = Map.mem numeric_keypad src in if not test then print_endline [%string "Bad numeric: %{src#Char}"]; test);
  assert (let test = Map.mem numeric_keypad dest in if not test then print_endline [%string "Bad numeric: %{dest#Char}"]; test);
  let key_pos = Map.find_exn numeric_keypad in
  let (sx,sy), (dx,dy) = key_pos src, key_pos dest in
  let x_moves = Int.abs (dx-sx) in
  let y_moves = Int.abs (dy-sy) in
  let delta_x = sign (dx-sx), 0 in
  let delta_y = 0, sign (dy-sy) in
  let gap = (-2, 0) in
  let is_valid_path path = 
    let rec loop pos = function
    | [] -> true
    | elt::rest -> 
        let pos = add_points pos elt in
        if IntTuple.equal gap pos 
          then false
          else loop pos rest in
    loop (sx,sy) path in
  combinations x_moves y_moves delta_x delta_y 
  |> List.filter ~f:is_valid_path
  |> List.map ~f:(List.map ~f:to_direction)

let directional_keypad_shortest_path src dest = 
  let rec calc src dest =
    assert (let test = Map.mem directional_keypad src in if not test then print_endline [%string "Bad direction: %{src#Char}"]; test);
    assert (let test = Map.mem directional_keypad dest in if not test then print_endline [%string "Bad direction: %{dest#Char}"]; test);
    match (src, dest) with
    | _ when Char.(src = dest) -> []
    | ('A', '^') -> ['<']
    | ('A', '<') -> ['v';'<';'<']
    | ('A', 'v') -> ['v';'<']
    | ('A', '>') -> ['v']
    | ('^', '<') -> ['v';'<']
    | ('^', 'v') -> ['v']
    | ('^', '>') -> ['>';'v']
    | ('>', '^') -> ['^';'<']
    | ('<', 'v') -> ['>']
    | ('<', '>') -> ['>';'>']
    | ('v', '>') -> ['>']
    | _ -> calc dest src |> List.map ~f:rev_direction |> List.rev in
  calc src dest

let keypad_presses_1 (shortest_path: shortest_path_fn) (code: char list): path list =
  let presses, _ = List.fold code ~init:([[]],'A') ~f:(fun (presses,src) key ->
    let path = shortest_path src key in 
    List.map presses ~f:(fun press ->
      press @ path @ ['A']
    ), key
  ) in
  presses |> List.min_elt ~compare:(on List.length Int.compare) |> Option.value_map ~f:List.singleton ~default:[]

(* Gives all the paths that can make a string *)
let keypad_presses (shortest_paths: shortest_paths_fn) (code: char list): path list =
  let presses, _ = List.fold code ~init:([[]],'A') ~f:(fun (presses,src) key ->
    let paths = shortest_paths src key in 
    List.concat_map paths ~f:(fun path -> 
      List.map presses ~f:(fun press ->
        press @ path @ ['A']
      )
    ), key
  ) in
  presses

let merge_keyboard_presses 
  (levels: int)
  (directional_keypad_shortest_path: shortest_path_fn) 
  (numeric_keypad_shortest_paths: shortest_paths_fn) 
  (code: char list): path list =
  let paths = ref (keypad_presses numeric_keypad_shortest_paths code) in
  for i = 1 to levels do
    paths := List.concat_map (!paths) ~f:(keypad_presses_1 directional_keypad_shortest_path);
    let lengths_str = String.concat ~sep:";" (List.map (!paths) ~f:(List.length >> Int.to_string)) in
    print_endline [%string "Level %{i#Int} has %{List.length !paths#Int} paths of lengths [%{lengths_str}]"];
  done;
  !paths

let find_shortest_path merge_keyboard_presses code = 
  let f = merge_keyboard_presses directional_keypad_shortest_path numeric_keypad_shortest_paths in
  let paths = f code in
  List.min_elt paths ~compare:(on List.length Int.compare) |> Option.value_exn

let numeric_part code = 
  String.of_list code |> Fn.flip String.prefix 3 |> Int.of_string

let complexity merge_keyboard_presses code =
  let path = find_shortest_path merge_keyboard_presses code in
  let numeric_part = numeric_part code in
  numeric_part * (List.length path)

let part_a filename = 
  read_codes filename 
  |> List.sum (module Int) ~f:(complexity (merge_keyboard_presses 2))
  
let part_b filename = 
  read_codes filename 
  |> List.sum (module Int) ~f:(complexity (merge_keyboard_presses 25))
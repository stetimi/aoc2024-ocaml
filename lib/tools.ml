open Core

let (>>) f g x = g (f x)

let set_printer (elt_printer: 'a -> string) s: string =
  let contents = Set.to_list s 
  |> List.map ~f:elt_printer
  |> String.concat ~sep:"," in
  [%string "(%{contents})"]

let map_printer (k_printer: 'k -> string) (v_printer: 'v -> string) (m: ('k, 'v, _) Map.t): string =
  Map.to_alist m
  |> List.map ~f:(fun (k, v) -> [%string "(%{k_printer k}->%{v_printer v})"])
  |> String.concat ~sep:","

let list_printer (elt_printer: 'a -> string) (xs: 'a list) =
  String.concat ~sep:";" @@ List.map xs ~f:elt_printer

let list_split_all xs ~f =
  let rec go xs acc = (
    match List.split_while xs ~f with
    | first, [] -> first :: acc
    | first, _ :: rest -> go rest (first :: acc)
  ) in go xs [] |> List.rev

let int_pair_printer (x,y) = [%string "(%{x#Int},%{y#Int})"]

let int_pair_list_printer xys = String.concat ~sep:";" @@ List.map ~f:int_pair_printer xys

let dfs ~(init:'a) ~(next: 'a -> 'a list) ~(is_target:'a -> bool): 'a list =
  let items = Stack.singleton init in
  let push_all = List.iter ~f:(Stack.push items) in
  let rec go found =
    Option.value_map (Stack.pop items)
      ~default:found
      ~f:(fun curr ->
        push_all @@ next curr;
        go @@ if is_target curr then curr :: found else found
      ) in
  go []

let bfs ~(init:'a) ~(next: 'a -> 'a list) ~(is_target:'a -> bool): 'a list =
  let items = Queue.singleton (init, []) in
  let push_all = List.iter ~f:(Queue.enqueue items) in
  let rec go () = (
    match Queue.dequeue items with
    | None -> []
    | Some (item, path) ->
      if is_target item
          then path
          else (
            let nexts = next item in
            let item_paths = List.map nexts ~f:(fun next -> next, next :: path) in
            push_all item_paths;
            go ()
          )
  ) in
  go ()


let track_seen (seen: 'a Hash_set.t) (next: 'a -> 'a list) (here: 'a): 'a list =
  let next_values = next here |> List.filter ~f:(Fn.non @@ Hash_set.mem seen) in
  List.iter next_values ~f:(Hash_set.add seen);
  next_values

module IntTuple = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) = 
    match Int.compare x0 x1 with
      0 -> Int.compare y0 y1
    | c -> c

  let compare_x_first = compare
  let compare_x_last t1 t2 = compare t1 t2 |> Int.neg
  let compare_y_first (x0, y0) (x1, y1) = compare (y0, x0) (y1, x1)
  let compare_y_last t1 t2 = compare_y_first t1 t2 |> Int.neg

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
  let hash (x,y) = Int.shift_left (Int.hash x) 16 + Int.hash y
end

module IntTupleSet = Set.Make(IntTuple)

let show_grid (grid: 'a array array) ~(f: 'a -> char): string =
  let show_row = Array.map ~f >> String.of_array in
  let rows = Array.map grid ~f:show_row in
  String.concat_array rows ~sep:"\n"

exception Found of int * int

let find_in_grid (grid: 'a array array) ~(f: 'a -> bool): (int * int) option =
  let width = Array.length grid.(0) in
  let height = Array.length grid in
  try
    for y = 0 to (height - 1) do
      for x = 0 to (width - 1) do
        if f (grid.(y).(x))
          then raise (Found (x,y))
      done
    done;
    None
  with Found (x,y) -> Some (x,y)

let add_points (s,t) (x,y) = (s+x, t+y)

let surroundings (max_x, max_y) (x,y) =
  let include_point (x,y) = if x >= 0 && x < max_x && y >= 0 && y < max_y then Some (x,y) else None in
  [0,-1;1,0;0,1;-1,0] |> List.filter_map ~f:(add_points (x,y) >> include_point)

type direction = N | E | S | W
[@@deriving ord, show]

let to_point = function N -> 0,1 | E -> 1,0 | S -> 0,-1 | W -> -1,0

let turn_right = function N -> E | E -> S | S -> W | W -> N

let turn_left = function N -> W | W -> S | S -> E | E -> N

let add_direction p d = add_points p @@ to_point d

let grid_at (grid: 'a array array) (x, y): 'a option =
  Option.some_if (x >= 0 && y >= 0 && y < (Array.length grid) && x < (Array.length grid.(0))) grid.(y).(x)

let mk_iter (next: 'v -> 'v list) =
  let iter v = (
    let stack = Stack.singleton v in
    let f k = match Stack.pop stack with 
    | None -> ()
    | Some v ->
        let nexts = next v in
        List.iter nexts ~f:(Stack.push stack);
        let e = v, v in
        k (e, v) in
    f
  ) in
  iter

let dijkstra grid_at n s e = 
  let dist = Array.init n ~f:(Fn.const Int.max_value) in
  let q = ref IntTupleSet.(singleton (0,s)) in
  dist.(s) <- 0;
  while not (Set.is_empty !q) do
    let (d, u) = Set.min_elt_exn !q in
    q := Set.remove !q (d, u);
    List.iter
      ~f:(fun (v, w) -> 
        let newdist = dist.(u) + w in
        if newdist < dist.(v) then
        begin
          q := Set.add !q (newdist, v);
          dist.(v) <- newdist
        end
      )
      grid_at u
  done;
  dist.(e)
    

  
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
  
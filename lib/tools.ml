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


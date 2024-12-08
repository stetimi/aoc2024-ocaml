open Core
open Utils

type ordering_rule = {before: int; after: int}
type update = int list

let read_input (filename: string): (ordering_rule list * update list) =
  let lines = In_channel.read_lines filename in
  let (ordering_rules_strs, updates_strs)  = List.split_while lines  ~f:(String.is_empty >> not) in
  let updates_strs = List.tl_exn updates_strs in
  let parse_ordering_rule_str s = (
    let [@warning "-8"] [before; after] = String.split s ~on:'|' |> List.map ~f:(Int.of_string)  in
    {before; after}
  ) in
  let ordering_rules = List.map ordering_rules_strs ~f:parse_ordering_rule_str in
  let parse_update_str s = String.split s ~on:',' |> List.map ~f:(Int.of_string) in
  let updates = List.map updates_strs ~f:parse_update_str in
  (ordering_rules, updates)

let to_map ordering_rules: (int, (int,Int.comparator_witness) Set.t, Int.comparator_witness) Map.t =
  let update m {before; after} = (
    Map.update m after ~f:(function
    | None -> Set.singleton (module Int) before
    | Some vs -> Set.add vs before
    )
  ) in
  List.fold_left ordering_rules ~init:(Map.empty (module Int)) ~f:update

let to_index_map =
  List.mapi ~f:(fun i update -> update, i)
  >> Map.of_alist_exn (module Int)

let middle_value update = 
  let len = List.length update in
  assert (Int.rem len 2 = 1);
  List.nth_exn update (len / 2)

let is_page_well_ordered ordering_rules_map updates_to_indices page = 
  let index = Map.find_exn updates_to_indices page in
  Map.find ordering_rules_map page 
  |> Option.map ~f:(Set.for_all ~f:(fun after -> 
    Map.find updates_to_indices after
    |> Option.map ~f:(fun before_index -> before_index < index)
    |> Option.value ~default:true
  ))
  |> Option.value ~default:true

let part_a_update_score ordering_rules_map update =
  let updates_to_indices = to_index_map update in
  if List.for_all update ~f:(is_page_well_ordered ordering_rules_map updates_to_indices) 
    then middle_value update else 0

let sort_by_ordering_rules ordering_rules_map (update: update) = 
  List.sort update ~compare:(fun x y -> 
    let befores = Map.find ordering_rules_map x |> Option.value ~default:(Set.empty (module Int)) in
    if Set.mem befores y then 1 else -1
  )

let part_b_update_score ordering_rules_map update =
  let updates_to_indices = to_index_map update in
  if List.for_all update ~f:(is_page_well_ordered ordering_rules_map updates_to_indices)
    then 0 
    else 
      let sorted = sort_by_ordering_rules ordering_rules_map update in
      middle_value sorted

let part_a filename = 
  let (ordering_rules, updates) = read_input filename in
  let ordering_rules_map = to_map ordering_rules in
  List.sum (module Int) updates ~f:(part_a_update_score ordering_rules_map)
  
let part_b filename =
  let (ordering_rules, updates) = read_input filename in
  let ordering_rules_map = to_map ordering_rules in
  List.sum (module Int) updates ~f:(part_b_update_score ordering_rules_map)

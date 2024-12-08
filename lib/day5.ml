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

let to_map (ordering_rules: ordering_rule list) =
  let update m {before; after} = (
    Map.update m before ~f:(function
    | None -> Set.singleton (module Int) after
    | Some vs -> Set.add vs after
    )
  ) in
  let empty = Map.empty (module Int) in
  List.fold_left ordering_rules ~init:empty ~f:update

let part_a filename = 
  let (_ordering_rules, _updates) = read_input filename in
  0
  
let part_b _filename = 0

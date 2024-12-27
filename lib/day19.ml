
open Core
open Tools

type puzzle = {
  patterns: string list;
  designs: string list;
}

let read_puzzle filename =
  let  [@warning "-8"] (patterns_str :: _, _ :: designs) = In_channel.read_lines filename 
  |> List.split_while ~f:(Fn.non String.is_empty) in
  let patterns = String.split patterns_str ~on:',' |> List.map ~f:(String.substr_replace_first ~pattern:" " ~with_:"") in
  {patterns; designs}

let can_match_design patterns design =
  let go recur design = 
    match design with
    | "" -> true
    | design -> 
        List.exists patterns ~f:(fun pattern -> 
          String.is_prefix design ~prefix:pattern && recur (String.drop_prefix design (String.length pattern))
        ) in
  let go = memoize (module String) go in
  go design

let count_matches patterns design =
  let go recur design = 
    match design with
    | "" -> 1
    | design -> 
        List.sum (module Int) patterns ~f:(fun pattern -> 
          if String.is_prefix design ~prefix:pattern 
            then recur (String.drop_prefix design (String.length pattern))
            else 0
        ) in
  let go = memoize (module String) go in
  go design
  
let part_a filename =
  let {patterns; designs} = read_puzzle filename in
  List.count designs ~f:(can_match_design patterns)

let part_b filename = 
  let {patterns; designs} = read_puzzle filename in
  List.sum (module Int) designs ~f:(count_matches patterns)


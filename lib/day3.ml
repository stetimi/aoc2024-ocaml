open Core

let sum_muls re text =
  let matches = Re.all re text |> List.map ~f:Re.Group.all in
  let read_match (total, enabled) (matched_array: string array) = (
    match (enabled, matched_array.(0)) with 
    | (_, "do()") -> (total, true)
    | (_,"don't()") -> (total, false)
    | (false, _) -> (total, enabled)
    | (true, _) -> 
      let first = matched_array.(1) |> Int.of_string_opt in
      let second = matched_array.(2) |> Int.of_string_opt in
      match (first, second) with 
      | (Some first, Some second) -> (total+first*second, enabled)
      | _ -> (total, enabled)
  ) in
  List.fold_left matches ~init:(0, true) ~f:read_match |> fst

let re_mul =
  let re_digits = Re.(rep1 digit) in
  Re.(seq [str "mul("; group re_digits; char ','; group re_digits; str ")"])

let part_a_sum_muls = sum_muls (Re.compile re_mul)

let part_b_sum_muls = 
  let re_do = Re.str "do()" in
  let re_dont = Re.str "don't()" in
  let re_expr = Re.alt [re_mul; re_do; re_dont] in  
  sum_muls (Re.compile re_expr)

let part_a filename =
  In_channel.read_all filename |> part_a_sum_muls

let part_b filename =
  In_channel.read_all filename |> part_b_sum_muls

open! Core
open Tools
open Timings

let parse_reports: string -> int list =
  String.split_on_chars ~on:[' '] 
  >> List.filter ~f:(String.is_empty |> Fun.negate)
  >> List.map ~f:(Int.of_string)


let read_reports filename: int list list = 
  In_channel.read_lines filename |> List.map ~f:parse_reports

let sign n = Int.compare n 0

let diff xs =
  List.zip_exn (List.drop_last_exn xs) (List.tl_exn xs) 
  |> List.map ~f:(fun (x, y) -> x - y)

let is_safe report =
  let diff_list = diff report in 
  let first = List.hd_exn diff_list in
  let sign_first = sign first in
  let is_safe n = sign_first <> 0 && (sign n = sign_first) && (Int.abs n >= 1) && (Int.abs n <= 3) in 
  List.for_all diff_list ~f:is_safe

let list_without xs i =
  let (before, after) = List.split_n xs i in
  List.concat [before; (List.tl_exn after)]

let is_safe_with_tolerance report =
  if is_safe report then true
  else
    List.range 0 (List.length report) |> List.exists ~f:(list_without report >> is_safe)

let part_a =
  List.count ~f:is_safe

let part_b =
  List.count ~f:is_safe_with_tolerance

let solve = solve read_reports part_a part_b

open! Core
open Tools
open Timings

let parse_reports: string -> int list =
  String.split_on_chars ~on:[' ']
  >> List.filter ~f:(String.is_empty |> Fn.non)
  >> List.map ~f:(Int.of_string)

let read_reports filename: int list list =
  In_channel.read_lines filename |> List.map ~f:parse_reports

let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

let diff xs =
  let zipped, _ = List.zip_with_remainder xs (List.tl_exn xs) in
  List.map zipped ~f:(fun (x, y) -> x - y)

let is_safe report remove_index =
  let diff_list = diff report in
  let first = List.hd_exn diff_list in
  let sign_first = sign first in
  let is_safe n = sign_first <> 0 && (sign n = sign_first) && let abs_n = Int.abs n in (abs_n >= 1) && (abs_n <= 3) in
  List.for_alli diff_list ~f:(fun i n -> i = remove_index || is_safe n)

let is_safe_with_tolerance report =
  Sequence.range 0 (List.length report) |> Sequence.exists ~f:(is_safe report)

let part_a =
  List.count ~f:(Fn.flip is_safe (-1))

let part_b =
  List.count ~f:is_safe_with_tolerance

let solve = solve read_reports part_a part_b

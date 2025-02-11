open! Core
open Aoc2024
open Timings

let to_millis ns =
  let millis = Int63.to_float ns /. 1_000_000. in
  sprintf "%.2f" millis

let print_solution (day: int) ~(printer: 'a -> string) ~time_taken solve =
  let input = [%string "inputs/day%{day#Int}.txt"] in
  let {part_a; part_b; shared_nanos} = solve input in
  time_taken := Int63.(!time_taken + (part_a.nanos + part_b.nanos + shared_nanos));
  print_endline [%string "Day %{day#Int}: shared: %{to_millis shared_nanos}ms part A: %{printer part_a.result} [%{to_millis part_a.nanos}ms], part B: %{printer part_b.result} [%{to_millis part_b.nanos}ms]"]

let days time_taken =
  let unsolved () = () in
  let print_int64_solution day solve () = print_solution day ~printer:Int64.to_string ~time_taken solve in
  let print_int_solution day solve () = print_solution day ~printer:Int.to_string ~time_taken solve in
  let print_string_solution day solve () = print_solution day ~printer:Fn.id ~time_taken solve in [|
    Day1.(print_int_solution 1 solve);
    Day2.(print_int_solution 2 solve);
    Day3.(print_int_solution 3 solve);
    Day4.(print_int_solution 4 solve);
    Day5.(print_int_solution 5 solve);
    Day6.(print_int_solution 6 solve);
    Day7.(print_int_solution 7 solve);
    Day8.(print_int_solution 8 solve);
    Day9.(print_int_solution 9 solve);
    Day10.(print_int_solution 10 solve);
    Day11.(print_int_solution 11 solve);
    Day12.(print_int_solution 12 solve);
    Day13.(print_int_solution 13 solve);
    Day14.(print_int_solution 14 solve);
    Day15.(print_int_solution 15 solve);
    Day16.(print_int_solution 16 solve);
    Day17.(print_string_solution 17 solve);
    unsolved;
    Day19.(print_int_solution 19 solve);
    Day20.(print_int_solution 20 solve);
    Day21.(print_int_solution 21 solve);
    Day22.(print_int_solution 22 solve);
    Day23.(print_string_solution 23 solve);
    Day24.(print_int64_solution 24 solve);
    Day25.(print_int_solution 25 solve);
  |]

let () =
  let args = Sys.get_argv () in
  let time_taken = ref (Int63.zero) in
  let days = if Array.length args = 2
    then
      let days = days time_taken in [|days.(Int.of_string args.(1) - 1)|]
      else days time_taken in
  Array.iter days ~f:(fun f -> f ());
  print_endline [%string "Total time taken was %{to_millis !time_taken}ms"]

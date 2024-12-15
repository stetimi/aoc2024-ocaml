open Core
open Aoc2024
open Tools

let time_ns = Time_ns.now >> Time_ns.to_int_ns_since_epoch

let timed_millis f input =
  let start = time_ns () in
  let result = f input in
  let finish = time_ns () in
  result, (finish - start) / 1_000_000

let print_result (k: int -> unit) (day: int) first second =
  let input = [%string "inputs/day%{day#Int}.txt"] in
  let first_result, first_millis = timed_millis first input in
  let second_result, second_millis = timed_millis second input in
  print_endline [%string "Day %{day#Int}: part A: %{first_result#Int} [%{first_millis#Int}ms], part B: %{second_result#Int} [%{second_millis#Int}ms]"];
  let _ = k @@ first_millis + second_millis in
  ()

let total_time (time_taken: int ref) (t: int) = 
  time_taken := !time_taken + t

let () = 
  let time_taken = ref 0 in
  let print_result = print_result @@ total_time time_taken in
  print_result 1 Day1.part_a Day1.part_b;
  print_result 2 Day2.part_a Day2.part_b;
  print_result 3 Day3.part_a Day3.part_b;  
  print_result 4 Day4.part_a Day4.part_b;
  print_result 5 Day5.part_a Day5.part_b;
  print_result 6 Day6.part_a Day6.part_b;
  print_result 7 Day7.part_a Day7.part_b;  
  print_result 8 Day8.part_a Day8.part_b;
  print_result 9 Day9.part_a Day9.part_b;
  print_result 10 Day10.part_a Day10.part_b;
  print_result 13 Day13.part_a Day13.part_b;
  print_endline [%string "Total time taken was %{!time_taken#Int}ms"]
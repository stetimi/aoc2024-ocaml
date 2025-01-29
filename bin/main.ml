open Core
open Aoc2024
open Tools

let time_ns = Time_ns.now >> Time_ns.to_int_ns_since_epoch

let timed_millis f input =
  let start = time_ns () in
  let result = f input in
  let finish = time_ns () in
  result, (finish - start) / 1_000_000

let print_result (k: int -> unit) (day: int) ~(printer: 'a -> string) first second =
  let input = [%string "inputs/day%{day#Int}.txt"] in
  let first_result, first_millis = timed_millis first input in
  let second_result, second_millis = timed_millis second input in
  print_endline [%string "Day %{day#Int}: part A: %{printer first_result} [%{first_millis#Int}ms], part B: %{printer second_result} [%{second_millis#Int}ms]"];
  let _ = k @@ first_millis + second_millis in
  ()

let total_time (time_taken: int ref) (t: int) = 
  time_taken := !time_taken + t

let ignore = Fn.const

let () = 
  let time_taken = ref 0 in
  let print_string_result = print_result ~printer:Fn.id @@ total_time time_taken in
  let print_int64_result = print_result ~printer:Int64.to_string @@ total_time time_taken in
  let print_result = print_result ~printer:Int.to_string @@ total_time time_taken in
  Day1.(print_result 1 part_a part_b);
  Day2.(print_result 2 part_a part_b);
  Day3.(print_result 3 part_a part_b);  
  Day4.(print_result 4 part_a part_b);
  Day5.(print_result 5 part_a part_b);
  Day6.(print_result 6 part_a part_b);
  Day7.(print_result 7 part_a part_b);  
  Day8.(print_result 8 part_a part_b);
  Day9.(print_result 9 part_a part_b);
  Day10.(print_result 10 part_a part_b);
  Day11.(print_result 11 part_a part_b);
  Day12.(print_result 12 part_a part_b);
  Day13.(print_result 13 part_a part_b);
  Day14.(print_result 14 (part_a (101, 103)) (ignore 0));
  Day15.(print_result 15 part_a part_b);
  Day16.(print_result 16 part_a part_b);
  Day17.(print_string_result 17 part_a part_b);
  (* Day18.(print_string_result 18 (part_a >> Int.to_string) part_b); *)
  Day19.(print_result 19 part_a part_b);
  Day20.(print_result 20 (part_a 100) (part_b 100));
  Day21.(print_result 21 part_a part_b);
  Day22.(print_result 22 part_a part_b); 
  Day23.(print_string_result 23 (part_a >> Int.to_string) part_b);
  Day24.(print_int64_result 24 part_a part_b); 
  Day25.(print_result 25 part_a part_b); 
  print_endline [%string "Total time taken was %{!time_taken#Int}ms"]
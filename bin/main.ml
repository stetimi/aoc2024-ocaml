open Core
open Aoc2024

let print_result (day: int) first second  =
  let input = [%string "inputs/day%{day#Int}.txt"] in
  print_endline [%string "Day %{day#Int}: part A: %{(first input)#Int}, part B: %{(second input)#Int}"]

let () = 
  print_result 1 Day1.part_a Day1.part_b;
  print_result 2 Day2.part_a Day2.part_b;
  print_result 3 Day3.part_a Day3.part_b;  
  print_result 4 Day4.part_a Day4.part_b;
  print_result 5 Day5.part_a Day5.part_b;
  (* print_result 6 Day6.part_a Day6.part_b; *)
  print_result 7 Day7.part_a Day7.part_b;  
  print_result 8 Day8.part_a Day8.part_b;
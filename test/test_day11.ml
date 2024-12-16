open OUnit2
open Aoc2024
open Day11
open Assertions

let day11_tests = "day 11 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 55312 (part_a "./test_inputs/day11.txt"));
  (* "part b sample" >:: (fun _ -> assert_int_equals 0 (part_b "./test_inputs/day11.txt")); *)
]

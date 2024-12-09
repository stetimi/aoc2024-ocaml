open OUnit2
open Aoc2024
open Assertions

let day6_tests = "day 6 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 41 (Day6.part_a "./test_inputs/day6.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 6 (Day6.part_b "./test_inputs/day6.txt"));
]

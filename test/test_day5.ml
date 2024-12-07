open OUnit2
open Aoc2024
open Assertions

let day5_tests = "day 5 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 143 (Day5.part_a "./test_inputs/day5.txt"));
]

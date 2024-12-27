open OUnit2
open Aoc2024
open Assertions
open Day19

let day19_tests = "day 19 test suite" >::: [
  "part a" >:: (fun _ -> assert_int_equals 6 (part_a "./test_inputs/day19.txt"));
  "part b" >:: (fun _ -> assert_int_equals 16 (part_b "./test_inputs/day19.txt"));
]

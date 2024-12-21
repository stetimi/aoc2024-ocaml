open OUnit2
open Aoc2024
open Assertions
open Day15

let day15_tests = "day 15 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 2028 (part_a "./test_inputs/day15.txt"));
]

open OUnit2
open Aoc2024
open Assertions

let day9_tests = "day 9 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 1928 (Day9.part_a "./test_inputs/day9.txt"));
]

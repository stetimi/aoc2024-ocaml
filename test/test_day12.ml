open OUnit2
open Aoc2024
open Assertions
open Day12

let day12_tests = "day 12 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 0 (part_a "./test_inputs/day12.txt"));
]

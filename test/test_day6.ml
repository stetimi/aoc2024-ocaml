open! Core
open OUnit2
open Aoc2024
open Assertions
open Day6

let day6_tests = "day 6 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 41 (part_a @@ read_map "./test_inputs/day6.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 6 (part_b @@ read_map "./test_inputs/day6.txt"));
]

open! Core
open OUnit2
open Aoc2024
open Assertions
open Day21

let day21_tests = "day 21 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 126384 (part_a @@ read_codes "./test_inputs/day21.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 154115708116294 (part_b @@ read_codes "./test_inputs/day21.txt"));
]

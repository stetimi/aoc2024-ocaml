open OUnit2
open Aoc2024
open Assertions
open Day22

let day22_tests = "day 22 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 0 (part_a "./test_inputs/day22.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 0 (part_b "./test_inputs/day22.txt"));
]

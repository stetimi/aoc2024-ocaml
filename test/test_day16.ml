open OUnit2
open Aoc2024
open Assertions
open Day16

let day16_tests = "day 16 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 7036 (part_a "./test_inputs/day16.txt"));
  (* "part b sample" >:: (fun _ -> assert_int_equals 45 (part_b "./test_inputs/day16.txt")); *)
]

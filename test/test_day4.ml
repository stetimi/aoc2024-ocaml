open OUnit2
open Aoc2024
open Assertions
open Day4

let day4_tests = "day 4 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 18 (part_a @@ read_grid "./test_inputs/day4.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 9 (part_b @@ read_grid "./test_inputs/day4.txt"));
]

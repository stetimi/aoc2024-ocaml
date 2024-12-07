open OUnit2
open Aoc2024
open Assertions

let day4_tests = "day 4 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 18 (Day4.part_a "./test_inputs/day4.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 9 (Day4.part_b "./test_inputs/day4.txt"));
]

open! Core
open OUnit2
open Aoc2024
open Assertions

let day9_tests = "day 9 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 1928 (Day9.part_a @@ In_channel.read_all "./test_inputs/day9.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 2858 (Day9.part_b @@ In_channel.read_all "./test_inputs/day9.txt"));
]

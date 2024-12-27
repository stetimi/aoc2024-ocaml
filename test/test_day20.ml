open OUnit2
open Aoc2024
open Assertions
open Day20

let day20_tests = "day 20 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 8 (part_a 12 "./test_inputs/day20.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 41 (part_b 69 "./test_inputs/day20.txt"));
]

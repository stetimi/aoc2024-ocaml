open OUnit2
open Aoc2024
open Assertions
open Day25

let day25_tests = "day 25 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 3 (part_a "./test_inputs/day25.txt"));
  (* "part b sample" >:: (fun _ -> assert_string_equals "co,de,ka,ta" (part_b "./test_inputs/day23.txt")); *)
]

open! Core
open OUnit2
open Aoc2024
open Assertions
open Day23

let day23_tests = "day 23 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 7 (part_a @@ read_lan_graph "./test_inputs/day23.txt"));
  "part b sample" >:: (fun _ -> assert_string_equals "co,de,ka,ta" (part_b @@ read_lan_graph "./test_inputs/day23.txt"));
]

open! Core
open OUnit2
open Aoc2024
open Assertions
open Day22

let day22_tests = "day 22 test suite" >::: [
  "part a sample" >:: (fun _ -> assert_int_equals 37327623 (part_a @@ read_secret_numbers "./test_inputs/day22.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 23 (part_b @@ read_secret_numbers "./test_inputs/day22_partb.txt"));
]

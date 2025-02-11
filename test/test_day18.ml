open! Core
open OUnit2
open Aoc2024
open Assertions
open Day18
open Tools

let day18_tests = "day 18 test suite" >::: [
  "part a sample" >:: (fun _ ->
    let corrupt_coords = corrupt_coords "./test_inputs/day18.txt" in
    let first_12 = List.take corrupt_coords 12 |> IntTupleSet.of_list in
    assert_int_equals 22 (path_length 7 first_12)
  );
  "part b" >:: (fun _ -> assert_string_equals "6,1" (part_b_search 7 13 "./test_inputs/day18.txt"));
]

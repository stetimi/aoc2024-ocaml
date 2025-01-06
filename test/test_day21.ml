open Core
open OUnit2
open Aoc2024
open Assertions
open Day21

let day21_tests = "day 21 test suite" >::: [
  "path order matters" >:: (fun _ ->
    find_shortest_path_str "456A" |> String.length |> assert_int_equals 64; 
  );
  "part a sample" >:: (fun _ -> assert_int_equals 126384 (part_a "./test_inputs/day21.txt"));
  (* "part b sample" >:: (fun _ -> assert_int_equals 0 (part_b "./test_inp  uts/day21.txt")); *)
]

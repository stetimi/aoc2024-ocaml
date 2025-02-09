open! Core
open OUnit2
open Aoc2024
open Assertions
open Day12
open Tools

let garden () = read_garden "test_inputs/day12.txt"

let day12_tests = "day 12 test suite" >::: [
  "find_region" >:: (fun _ ->
    let garden = garden () in
    let i_s = find_region garden (4,0) |> Hash_set.to_list |> List.sort ~compare:IntTuple.compare in
    assert_list_equals [4,0;4,1;5,0;5,1] i_s ~cmp:int_pair_equal ~printer:int_pair_printer
  );
  "part a sample" >:: (fun _ -> assert_int_equals 1930 (part_a @@ regions "./test_inputs/day12.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 1206 (part_b @@ regions "./test_inputs/day12.txt"));
]

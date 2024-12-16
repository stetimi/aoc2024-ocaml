open OUnit2
open Aoc2024
open Assertions
open Day10
open Tools

let grid = read_grid "./test_inputs/day10.txt"

let assert_pos_array_equal = assert_array_equal ~cmp:int_pair_equal ~printer:int_pair_printer

let assert_pos_list_equal = assert_list_equals ~cmp:int_pair_equal ~printer:int_pair_printer

let assert_pos_dir_list_equal = assert_list_equals ~cmp:int_pair_equal ~printer:int_pair_printer

let day10_tests = "day 10 test suite" >::: [
  "find_trailheads" >:: (fun _ ->
    assert_pos_array_equal
      [|2,0;4,0;4,2;6,4;2,5;5,5;0,6;6,6;1,7|] (find_trailheads grid)
  );
  "next_steps_in_trail" >:: (fun _ ->
    assert_pos_list_equal [3,3;2,2] (next_steps_in_trail grid (3,2))
  );
  "part a sample" >:: (fun _ -> assert_int_equals 36 (Day10.part_a "./test_inputs/day10.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 81 (Day10.part_b "./test_inputs/day10.txt"));
]

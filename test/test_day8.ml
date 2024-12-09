open Core
open OUnit2
open Aoc2024
open Assertions

let grid = Day8.read_map "test_inputs/day8.txt"

let day8_tests = "day 8 test suite" >::: [
  "pairs_of" >:: (fun _ -> 
    let pairs = Day8.pairs_of [1;2;3;4] |> Sequence.to_list in
    assert_list_equals [(1,2);(1,3);(1,4);(2,3);(2,4);(3,4)] pairs ~cmp:int_pair_equal ~printer:int_pair_printer
  );
  "pairs_of on singleton is empty" >:: (fun _ -> assert_bool "pairs of on singleton" (Day8.pairs_of [1] |> Sequence.is_empty));
  "antenna_positions" >:: (fun _ ->
    let antenna_positions = Day8.antenna_positions grid in
    assert_int_equals 2 (Map.length antenna_positions);
    let zeroes = Map.find_exn antenna_positions '0' in
    assert_list_equals [20;29;43;52] zeroes ~cmp:(=) ~printer:Int.to_string;
  );
  "part a sample" >:: (fun _ -> assert_int_equals 14 (Day8.part_a "./test_inputs/day8.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 34 (Day8.part_b "./test_inputs/day8.txt"));
]

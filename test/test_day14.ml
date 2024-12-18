open OUnit2
open Aoc2024
open Assertions
open Day14

let day14_tests = "day 14 test suite" >::: [
  "read_robot" >:: (fun _ ->
    let robot = read_robot "p=0,4 v=3,-3" in
    let expected = {position=0,4; velocity=3,(-3)} in
    assert_cmp_equal expected robot ~printer:show_robot ~cmp:compare_robot
  );
  "part a sample" >:: (fun _ -> assert_int_equals 12 (part_a (11, 7) "./test_inputs/day14.txt"));
]

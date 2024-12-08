open Core
open OUnit2
open Aoc2024
open Assertions

let day5_tests = "day 5 test suite" >::: [
  (* "part a sample" >:: (fun _ -> assert_int_equals 143 (Day5.part_a "./test_inputs/day5.txt")); *)
  "to_map" >:: (fun _ -> 
    let ordering_rules: Day5.ordering_rule list = [
      {before=1; after=10};
      {before=2; after=11};
      {before=1; after=12};
    ] in
    let expected = Map.of_alist_exn (module Int) [
      1, (Set.of_list (module Int) [10; 12]);
      2, (Set.singleton (module Int) 11);
    ] in
    let map = Day5.to_map ordering_rules in
    let printer = map_printer (Int.to_string) (set_printer Int.to_string) in
    assert_equal expected map ~cmp:(Map.equal (Set.equal)) ~printer
  );
]

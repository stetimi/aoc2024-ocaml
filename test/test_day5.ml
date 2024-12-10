open Core
open OUnit2
open Aoc2024
open Assertions
open Utils

let day5_tests = "day 5 test suite" >::: [
  "part a example" >:: (fun _ -> assert_int_equals 143 (Day5.part_a "./test_inputs/day5.txt"));
  "part b example" >:: (fun _ -> assert_int_equals 123 (Day5.part_b "./test_inputs/day5.txt"));
  "middle_value" >:: (fun _ -> assert_int_equals 47 (Day5.middle_value [75;97;47;61;53]));
  "to_index_map" >:: (fun _ ->
    let values = [75;97;47;61;53] in
    let index_map = Day5.to_index_map values in
    let expected = [75,0; 97,1; 47,2; 61,3; 53,4] |> Map.of_alist_exn (module Int) in
    let printer = map_printer (Int.to_string) (Int.to_string) in
    assert_equal expected index_map ~cmp:(Map.equal (Int.equal)) ~printer;
  );
  "to_map" >:: (fun _ -> 
    let ordering_rules: Day5.ordering_rule list = [
      {before=1; after=10};
      {before=2; after=11};
      {before=3; after=10};
    ] in
    let expected = Map.of_alist_exn (module Int) [
      10, (Set.of_list (module Int) [1; 3]);
      11, (Set.singleton (module Int) 2);
    ] in
    let map = Day5.to_map ordering_rules in
    let printer = map_printer (Int.to_string) (set_printer Int.to_string) in
    assert_equal expected map ~cmp:(Map.equal (Set.equal)) ~printer
  );
]

open OUnit2
open Aoc2024

let day3_tests = "day 3 test suite" >::: [
  "dummy" >:: (fun _ -> assert_equal 0 (Day3.part_a ""));
]

open OUnit2
open Aoc2024

let day1_tests = "day 1 test suite" >::: [
  "part ints works" >:: (fun _ -> assert_equal [10000; 20000] (Day1.parse_ints "10000   20000"))
]

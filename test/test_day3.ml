open OUnit2
open Aoc2024

let assert_int_equals = assert_equal ~printer: Int.to_string 

let day3_tests = "day 3 test suite" >::: [
  "sum_muls on string containing no mul()" >:: (fun _ -> assert_int_equals 0 (Day3.sum_muls "no mul(45,int) here"));
  "sum_muls on string containing one mul()" >:: (fun _ -> assert_int_equals 2520 (Day3.sum_muls "there is mul(45,56) here"));
  "sum_muls on string containing two mul()" >:: (fun _ -> assert_int_equals 41 (Day3.sum_muls "there mul(5,4) is mul(7,3) here"));
]

open OUnit2
open Aoc2024
open Assertions

let day3_tests = "day 3 test suite" >::: [
  "part a on string containing one mul()" >:: (fun _ -> assert_int_equals 2520 (Day3.part_a_sum_muls "there is mul(45,56) here"));
  "part a on string containing two mul()" >:: (fun _ -> assert_int_equals 41 (Day3.part_a_sum_muls "there mul(5,4) is mul(7,3) here"));
  "part b on example string" >:: (fun _ -> assert_int_equals 161 (Day3.part_b_sum_muls "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"));
]

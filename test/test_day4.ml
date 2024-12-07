open Core
open OUnit2
open Aoc2024
open Assertions

let day4_tests = "day 4 test suite" >::: [
  "count_xmas_matches" >:: (fun _ -> 
    assert_int_equals 5 (Day4.count_xmas_matches ("X XXMAS SAMX YMAS XMAS SAMXMAS" |> String.to_array))
  )
]

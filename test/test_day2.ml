open OUnit2
open Aoc2024

let assert_safe report = Day2.is_safe report |> assert_bool "should be safe"
let assert_unsafe report = Day2.is_safe report |> not |> assert_bool "should be unsafe"

let day2_tests = "day 2 test suite" >::: [
  "decreasing list is safe" >:: (fun _ -> assert_safe [4; 2; 1]);
  "increasing list is safe" >:: (fun _ -> assert_safe [2; 4; 5; 8]);
  "non-monotonic list is not safe" >:: (fun _ -> assert_unsafe [1; 3; 2; 3]);
  "list with gap larger than 3 not safe" >:: (fun _ -> assert_unsafe [1; 3; 6; 10; 12]);
  "flat list is not safe" >:: (fun _ -> assert_unsafe [1; 1]);
  "list with zero gap not safe" >:: (fun _ -> assert_unsafe [1; 1; 2; 3]);
  "list with zero gap at end not safe" >:: (fun _ -> assert_unsafe [1; 2; 3; 3]);
  "list_without removes first item of list" >:: (fun _ -> assert_equal [2; 3] (Day2.list_without [1; 2; 3] 0));
  "list_without removes middle item of list" >:: (fun _ -> assert_equal [1; 3] (Day2.list_without [1; 2; 3] 1));
  "list_without removes final item of list" >:: (fun _ -> assert_equal [1; 2] (Day2.list_without [1; 2; 3] 2))
]

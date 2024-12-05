open OUnit2

let () =
  run_test_tt_main (
    "AOC 2024 tests" >::: [
      Test_day1.day1_tests;
      Test_day2.day2_tests;
      Test_day3.day3_tests;
    ]
  )
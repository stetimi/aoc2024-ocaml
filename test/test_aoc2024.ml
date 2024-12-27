open OUnit2

let () =
  run_test_tt_main (
    "AOC 2024 tests" >::: [
      Test_day1.day1_tests;
      Test_day2.day2_tests;
      Test_day3.day3_tests;
      Test_day4.day4_tests;
      Test_day5.day5_tests;
      Test_day6.day6_tests;
      Test_day7.day7_tests;
      Test_day8.day8_tests;
      Test_day9.day9_tests;
      Test_day10.day10_tests;
      Test_day11.day11_tests;
      Test_day12.day12_tests;
      Test_day13.day13_tests;
      Test_day14.day14_tests;
      Test_day15.day15_tests;
      (* Test_day16.day16_tests; *)
      Test_day17.day17_tests;
      Test_day18.day18_tests;
      Test_day19.day19_tests;
      Test_grid.grid_tests;
      Test_tools.tools_tests;
    ]
  )
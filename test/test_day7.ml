open OUnit2
open Aoc2024
open Assertions

let day7_tests = "day 7 test suite" >::: [
  "extend when not meeting target" >:: (fun _ ->
    let extended = Day7.extend Day7.part_a_ops 100 [25; 80; 11] 3 true in
    assert_list_equals [28; 83; 14; 75; 33] extended ~cmp:(=) ~printer:Int.to_string
  );
  "extend when meeting target through multiplication" >:: (fun _ -> assert_raises Exit (fun () -> Day7.extend Day7.part_a_ops 100 [25; 50] 2 true));
  "extend when meeting target through addition" >:: (fun _ -> assert_raises Exit (fun () -> Day7.extend Day7.part_a_ops 100 [25; 80; 99] 1 true));
  "can be calibrated when meeting target" >:: (fun _ ->
    assert_bool "" @@ Day7.can_be_calibrated Day7.part_a_ops {target=292; numbers=[11;6;16;20]}
  );
  "can be calibrated when not meeting target" >:: (fun _ ->
    assert_bool "" (not @@ Day7.can_be_calibrated Day7.part_a_ops {target=292; numbers=[11;6;16;20;2]})
  );
  "can be calibrated bug" >:: (fun _ ->
    assert_bool "" @@ Day7.can_be_calibrated Day7.part_a_ops {target=2954988; numbers=[6; 3; 3; 16; 4; 95; 2; 1; 7; 3; 9; 3]}
  );
  "part a sample" >:: (fun _ -> assert_int_equals 3749 (Day7.part_a "./test_inputs/day7.txt"));
  "part b sample" >:: (fun _ -> assert_int_equals 11387 (Day7.part_b "./test_inputs/day7.txt"));
]

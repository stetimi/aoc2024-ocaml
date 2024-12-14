open OUnit2
open Aoc2024
open Assertions
open Day13

let machine_spec : machine_spec = {
  buttons = [|94.; 34.; 22.; 67.|];
  prize = [|8400.; 5400.|];
}

let day13_tests = "day 13 test suite" >::: [
  "read_button tuple" >:: (fun _ ->
    let button = read_tuple re_button "Button B: X+22, Y+67" in
    assert_equal (22., 67.) button ~cmp:float_pair_equal ~printer:float_pair_printer
  );
  "read prize tuple" >:: (fun _ ->
    let prize = read_tuple re_prize "Prize: X=7870, Y=6450" in
    assert_equal (7870., 6450.) prize ~cmp:float_pair_equal ~printer:float_pair_printer
  ); 
  "to_machine_spec" >:: (fun _ ->
    let lines = [
      "Button A: X+94, Y+34";
      "Button B: X+22, Y+67";
      "Prize: X=8400, Y=5400";
    ] in
    let actual_spec = to_machine_spec lines in
    assert_equal ~cmp:(fun s1 s2 -> compare_machine_spec s1 s2 = 0) ~printer:show_machine_spec actual_spec machine_spec
  );
  "calc_button_presses" >:: (fun _ ->
    let (a, b) = calc_button_presses machine_spec in
    assert_float_equals 80. a;
    assert_float_equals 40. b;
  );
  "part a sample" >:: (fun _ -> assert_int_equals 480 (part_a "./test_inputs/Day13.txt"));
]

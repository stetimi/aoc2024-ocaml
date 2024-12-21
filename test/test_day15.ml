open Core
open OUnit2
open Aoc2024
open Assertions
open Day15
(* open Tools *)

(*
01234567890123
##############0
##......##..##1
##..........##2
##.[][][]...##3
##....[]....##4
##....^.....##5
##############
*)  
let warehouse () = 
  let w = [
    "#######";
    "#...#.#";
    "#.....#";
    "#.OOO@#";
    "#..O..#";
    "#.....#";
    "#######";
    ] |> read_doubled_warehouse in
  Array.set w 3 [|Wall;Wall;Space;BoxLeft;BoxRight;BoxLeft;BoxRight;BoxLeft;BoxRight;Space;Space;Space;Wall;Wall|];
  w

let day15_tests = "day 15 test suite" >::: [
  "find_boxes going left" >:: (fun _ ->
    let warehouse = warehouse () in
    let boxes = find_boxes warehouse Left (9,3) in
    let expected = [BoxLeft,(3,3); BoxRight,(4,3); BoxLeft,(5,3); BoxRight,(6,3); BoxLeft,(7,3); BoxRight,(8,3)] in
    let cell_pos_eq = Tuple2.equal ~eq1:cell_eq ~eq2:int_pair_equal in
    let cell_pos_printer = tuple_printer ~p1:show_cell ~p2:int_pair_printer in
    assert_list_equals expected boxes ~cmp:cell_pos_eq ~printer:cell_pos_printer
  );
  "find_boxes going up" >:: (fun _ ->
    let warehouse = warehouse () in
    let boxes = find_boxes warehouse Up (6,5) in
    let expected = [BoxLeft,(5,3); BoxRight,(6,3); BoxLeft,(7,3); BoxRight,(8,3); BoxLeft,(6,4); BoxRight,(7,4)] in
    let cell_pos_eq = Tuple2.equal ~eq1:cell_eq ~eq2:int_pair_equal in
    let cell_pos_printer = tuple_printer ~p1:show_cell ~p2:int_pair_printer in
    assert_list_equals expected boxes ~cmp:cell_pos_eq ~printer:cell_pos_printer
  );
  "find_boxes going right" >:: (fun _ ->
    let warehouse = warehouse () in
    let boxes = find_boxes warehouse Right (2,3) in
    let expected = [BoxRight,(8,3); BoxLeft,(7,3); BoxRight,(6,3); BoxLeft,(5,3); BoxRight,(4,3); BoxLeft,(3,3)] in
    let cell_pos_eq = Tuple2.equal ~eq1:cell_eq ~eq2:int_pair_equal in
    let cell_pos_printer = tuple_printer ~p1:show_cell ~p2:int_pair_printer in
    assert_list_equals expected boxes ~cmp:cell_pos_eq ~printer:cell_pos_printer
  );
  "find_boxes going down" >:: (fun _ ->
    let warehouse = warehouse () in
    let boxes = find_boxes warehouse Down (8,2) in
    let expected = [BoxRight,(7,4); BoxLeft,(6,4); BoxRight,(8,3); BoxLeft,(7,3)] in
    let cell_pos_eq = Tuple2.equal ~eq1:cell_eq ~eq2:int_pair_equal in
    let cell_pos_printer = tuple_printer ~p1:show_cell ~p2:int_pair_printer in
    assert_list_equals expected boxes ~cmp:cell_pos_eq ~printer:cell_pos_printer
  );
  "part a sample" >:: (fun _ -> assert_int_equals 2028 (part_a "./test_inputs/day15.txt"));
  "part a large sample" >:: (fun _ -> assert_int_equals 10092 (part_a "./test_inputs/day15_large.txt"));
  "part b sample" >:: (fun _ -> 
    print_endline "";
    assert_int_equals 9021 (part_b "./test_inputs/day15_large.txt")
  );
]
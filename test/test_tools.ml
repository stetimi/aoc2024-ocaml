open Core
open OUnit2
open Aoc2024  
open Tools
open Assertions

let tools_tests = "tools test suite" >::: [
  "list_split_all" >:: (fun _ -> 
    let xs = ["1"; "2"; ""; "3"; ""; "4"] in
    let split = list_split_all xs ~f:(Fn.non String.is_empty) in
    assert_list_equals [["1"; "2"]; ["3"]; ["4"]] split 
      ~cmp:(List.equal String.(=)) 
      ~printer:(list_printer ~printer:Fn.id)
  );
  "surroundings top left" >:: (fun _ ->
    let around_top_left = surroundings (5,5) (0,0) in
    assert_list_equals [1,0;0,1] around_top_left ~cmp:int_pair_equal ~printer:int_pair_printer
  );
  "track_seen" >:: (fun _ ->
    let next = (function
    | 0 -> [10; 11]
    | 10 -> [11; 12]
    | _ -> failwith "") in
    let next = track_seen ~f:Fn.id (Hash_set.create (module Int)) next in
    assert_list_equals [10; 11] (next 0) ~cmp:(=) ~printer:(Int.to_string);
    assert_list_equals [12] (next 10) ~cmp:(=) ~printer:(Int.to_string);
    assert_list_equals [] (next 0) ~cmp:(=) ~printer:(Int.to_string);
  );
  "binary_chop" >:: (fun _ ->
    let f n = [|1; 3; 5; 7; 9|].(n) in
    let binary_chop = binary_chop ~f ~cmp:Int.compare ~low:0 ~high:4 in
    assert_equal None (binary_chop ~target:4);
    assert_equal (Some 1) (binary_chop ~target:3);
    assert_equal (Some 4) (binary_chop ~target:9);
  );
]



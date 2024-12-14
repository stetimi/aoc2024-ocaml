open Core
open OUnit2
open Aoc2024  

let tools_tests = "tools test suite" >::: [
  "list_split_all" >:: (fun _ -> 
    let xs = ["1"; "2"; ""; "3"; ""; "4"] in
    let split = Tools.list_split_all xs ~f:(Fn.non String.is_empty) in
    Assertions.assert_list_equals [["1"; "2"]; ["3"]; ["4"]] split 
      ~cmp:(List.equal String.(=)) 
      ~printer:(Assertions.list_printer ~printer:Fn.id)
  );
]



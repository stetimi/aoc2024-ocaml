open Core
open OUnit2
open Aoc2024  
open Assertions

let g = Grid.init 5 (Array.init 20 ~f:Fn.id)

let int_pair_equal = Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal

let int_pair_to_string (x,y) = sprintf "(%d,%d)" x y

let assert_int_pair_equal expected actual  =
  assert_equal expected actual ~cmp:int_pair_equal ~printer:int_pair_to_string

let grid_tests = "grid test suite" >::: [
  "rows" >:: (fun _ -> assert_equal 4 (Grid.rows g));
  "to_xy" >:: (fun _ -> assert_int_pair_equal (1,2) (Grid.to_xy 11 g));
  "points" >:: (fun _ -> 
    assert_array_equal [|(3,4);(4,3);(5,2)|] (Grid.points (3,4) (1,-1) 3)
     ~cmp:int_pair_equal ~printer:int_pair_to_string
  )
]



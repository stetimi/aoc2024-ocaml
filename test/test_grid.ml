open Core
open OUnit2
open Aoc2024  

let g = Grid.init 5 (Array.init 20 ~f:Fn.id)

let assert_int_array_equal (expected: int array) (actual: int array) = 
  assert_equal expected actual ~cmp:(Array.equal Int.equal) ~printer:(fun x -> Array.to_list x |> List.to_string ~f:(Int.to_string))

let assert_int_pair_equal expected actual  =
  assert_equal expected actual ~cmp:(Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal) ~printer:(fun (x, y) -> sprintf "(%d,%d)" x y)

let assert_int_equal expected actual = assert_equal ~cmp:Int.equal ~printer:Int.to_string expected actual

let grid_tests = "grid test suite" >::: [
  "rows" >:: (fun _ -> assert_equal 4 (Grid.rows g));
  "to_xy" >:: (fun _ -> assert_int_pair_equal (1, 2) (Grid.to_xy 11 g));
  "to_dxdy going down" >:: (fun _ -> assert_int_pair_equal (2, 1) (Grid.to_dxdy 2 7 g));
  "to_dxdy going up" >:: (fun _ -> assert_int_pair_equal (-1, -1) (Grid.to_dxdy 12 (-6) g));
  "rows" >:: (fun _ -> 
    let expected = [6; 7; 8; 9] |> List.to_array in
    assert_int_array_equal expected (Grid.line_from 6 1 g)
  );
  "line_from returning a row going to the left" >:: (fun _ -> 
    let expected = [8; 7; 6; 5] |> List.to_array in
    assert_int_array_equal expected (Grid.line_from 8 (-1) g)
  );
  "line_from returning a diagonal going from top left to bottom right" >:: (fun _ -> 
    let expected = [2; 8; 14] |> List.to_array in
    assert_int_array_equal expected (Grid.line_from 2 6 g)
  );
  "line_from returning a diagonal going from bottom left to top right" >:: (fun _ -> 
    let expected = [17; 13; 9] |> List.to_array in
    assert_int_array_equal expected (Grid.line_from 17 (-4) g)
  );
  "all_rows" >:: (fun _ ->
    let rows = Grid.all_rows g in
    assert_int_equal 4 (Array.length rows);
    assert_int_array_equal [|  0;  1;  2;  3;  4 |] rows.(0);
    assert_int_array_equal [|  5;  6;  7;  8;  9 |] rows.(1);
    assert_int_array_equal [| 10; 11; 12; 13; 14 |] rows.(2);
    assert_int_array_equal [| 15; 16; 17; 18; 19 |] rows.(3);
  );
  "all_cols" >:: (fun _ ->
    let cols = Grid.all_cols g in
    assert_int_equal 5 (Array.length cols);
    assert_int_array_equal [| 0; 5; 10; 15 |] cols.(0);
    assert_int_array_equal [| 1; 6; 11; 16 |] cols.(1);
    assert_int_array_equal [| 2; 7; 12; 17 |] cols.(2);
    assert_int_array_equal [| 3; 8; 13; 18 |] cols.(3);
    assert_int_array_equal [| 4; 9; 14; 19 |] cols.(4);
  );
  "all_tl_br_diagonals" >:: (fun _ ->
    let diagonals = Grid.all_tr_bl_diagonals g in
    assert_int_equal 8 (Array.length diagonals);
    assert_int_array_equal [|  0;            |] diagonals.(0);
    assert_int_array_equal [|  1;  5         |] diagonals.(1);
    assert_int_array_equal [|  2;  6; 10     |] diagonals.(2);
    assert_int_array_equal [|  3;  7; 11; 15 |] diagonals.(3);
    assert_int_array_equal [|  4;  8; 12; 16 |] diagonals.(4);
    assert_int_array_equal [|  9; 13; 17     |] diagonals.(5);
    assert_int_array_equal [| 14; 18;        |] diagonals.(6);
    assert_int_array_equal [| 19             |] diagonals.(7);
  );
]



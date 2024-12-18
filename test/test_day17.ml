open OUnit2
open Aoc2024
open Day17
open Assertions
open Core

let zero_registers () = {a=0;b=0;c=0}
let computer registers = {pc=0; registers;}

let day17_tests = "day 17 test suite" >::: [
  "parse_colon_sep" >:: (fun _ -> 
    let actual = parse_colon_sep "Register A: 729" ~convert:Int.of_string in
    assert_int_equals 729 actual
  );
  "read_input" >:: (fun _ ->
    let computer, program = Day17.read_input "./test_inputs/day17.txt" in
    assert_equal {a=729; b=0; c=0} computer.registers ~cmp:(fun r1 r2 -> compare_registers r1 r2 = 0) ~printer:show_registers;
    assert_array_equal [|0;1;5;4;3;0|] program ~cmp:(=) ~printer:Int.to_string;
  );
  "adv" >:: (fun _ ->
    let registers = {a=100; b=0; c=0} in
    adv registers 3;
    assert_int_equals 12 registers.a;
  );
  "bxl" >:: (fun _ ->
    let registers = {a=100; b=3786; c=0} in
    bxl registers 1984;
    assert_int_equals 2314 registers.b;
  );
  "bst" >:: (fun _ ->
    let registers = {a=0; b=0; c=71} in
    bst registers 6;
    assert_int_equals 7 registers.b;
  );
  "jnz if a is 0" >:: (fun _ ->
    let computer = computer (zero_registers ()) in
    jnz computer 20;
    assert_int_equals 0 computer.pc;
  );
  "jnz if a is not 0" >:: (fun _ ->
    let registers = {a=20; b=0; c=0} in
    let computer = computer registers in
    jnz computer 10;
    assert_int_equals 10 computer.pc;
  );
  "bxc" >:: (fun _ ->
    let registers = {a=0; b=1984; c=3786} in
    bxc registers;
    assert_int_equals 2314 registers.b;
  );
  "out" >:: (fun _ ->
    let registers = zero_registers () in
    let storage = ref 0 in
    out registers (fun n -> storage := n) 3;
    assert_int_equals 3 !storage;
  );
  "self copy" >:: (fun _ ->
    let registers = {a=117440; b=0; c=0} in
    let computer = computer registers in
    let program = [|0;3;5;4;3;0|] in
    let result = run computer program in
    assert_array_equal program result ~cmp:(=) ~printer:Int.to_string
  );
  "part a sample" >:: (fun _ -> assert_string_equals "4,6,3,5,6,3,5,2,1,0" (part_a "./test_inputs/day17.txt"));
]

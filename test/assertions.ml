open Core
open OUnit2

let assert_int_equals = assert_equal ~printer: Int.to_string 

let assert_array_equal (expected: 'a array) (actual: 'a array) ~(cmp:'a -> 'a -> bool) ~(printer: 'a -> string) =
  assert_equal (Array.length expected) (Array.length actual) ~cmp:Int.equal ~printer:Int.to_string ~msg:"Lengths differ";
  let expected_actual = Array.zip_exn expected actual in 
  Array.iteri expected_actual ~f:(fun i (e, a) ->  assert_equal e a ~cmp ~printer:(fun x -> sprintf "[%d]: %s" i (printer x)));
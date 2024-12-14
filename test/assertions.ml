open Core
open OUnit2

let assert_int_equals = assert_equal ~printer:Int.to_string ~cmp:(=)

let assert_float_equals = assert_equal ~printer:Float.to_string ~cmp:Float.(=)

let assert_list_equals (expected: 'a list) (actual: 'a list) ~(cmp:'a -> 'a -> bool) ~(printer: 'a -> string) =
  assert_int_equals (List.length expected) (List.length actual) ~msg:"Lengths differ";
  let expected_actual = List.zip_exn expected actual in 
  let diffs = List.filter_mapi expected_actual ~f:(fun i (e, a) ->
    Option.some_if (not @@ cmp e a) [%string "Index %{i#Int}: Expected: %{printer e}, actual: %{printer a}"]
  ) in
  if not @@ List.is_empty diffs then 
    let diffs_str = String.concat diffs ~sep:"\n" in
    assert_failure diffs_str;
  else
    ()

let assert_array_equal (expected: 'a array) (actual: 'a array) ~(cmp:'a -> 'a -> bool) ~(printer: 'a -> string) =
  assert_int_equals (Array.length expected) (Array.length actual) ~msg:"Lengths differ";
  let expected_actual = Array.zip_exn expected actual in 
  Array.iteri expected_actual ~f:(fun i (e, a) -> assert_equal e a ~cmp ~printer:(fun x -> [%string "%{i#Int}: %{printer x}"]))

let int_pair_equal = Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal

let int_pair_printer (x,y) = [%string "(%{x#Int},%{y#Int})"]

let list_printer ~printer xs =
  String.concat ~sep:";" @@ List.map xs ~f:printer

let float_pair_equal = Tuple2.equal ~eq1:Float.equal ~eq2:Float.equal

let float_pair_printer (x,y) = [%string "(%{x#Float},%{y#Float})"]

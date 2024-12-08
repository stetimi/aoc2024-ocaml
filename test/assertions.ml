open Core
open OUnit2

let assert_int_equals = assert_equal ~printer:Int.to_string ~cmp:(=)

let assert_array_equal (expected: 'a array) (actual: 'a array) ~(cmp:'a -> 'a -> bool) ~(printer: 'a -> string) =
  assert_equal (Array.length expected) (Array.length actual) ~cmp:Int.equal ~printer:Int.to_string ~msg:"Lengths differ";
  let expected_actual = Array.zip_exn expected actual in 
  Array.iteri expected_actual ~f:(fun i (e, a) ->  assert_equal e a ~cmp ~printer:(fun x -> [%string "%{i#Int}: %{printer x}"]))

let set_printer (elt_printer: 'a -> string) s: string =
  let contents = Set.to_list s 
  |> List.map ~f:elt_printer
  |> String.concat ~sep:"," in
  [%string "(%{contents})"]

let map_printer (k_printer: 'k -> string) (v_printer: 'v -> string) m: string =
  Map.to_alist m
  |> List.map ~f:(fun (k, v) -> [%string "(%{k_printer k}->%{v_printer v})"])
  |> String.concat ~sep:","

open! Core
open Timings

type machine_spec = {
  buttons: float array;
  prize: float array;
}
[@@deriving ord, show]

let re_digits = Re.(rep1 digit) 

let re_button =
  Re.(seq [str "Button "; alpha; str ": X+"; group re_digits; str ", Y+"; group re_digits]) |> Re.compile

let re_prize =
  Re.(seq [str "Prize: X="; group re_digits; str ", Y="; group re_digits]) |> Re.compile

let read_tuple re line =
  let [@warning "-8"] buttons :: _ = Re.all re line |> List.map ~f:Re.Group.all in
  Float.of_string buttons.(1), Float.of_string buttons.(2)

let to_machine_spec (lines: string list): machine_spec = 
  let [@warning "-8"] [button_a; button_b; prize] = lines in
  let read_button = read_tuple re_button in
  let button_a = read_button button_a in
  let button_b = read_button button_b in
  let prize = read_tuple re_prize prize in
  {
    buttons = [|fst button_a; snd button_a; fst button_b; snd button_b|];
    prize = [|fst prize; snd prize|];
  }

let read_machine_specs filename =
  In_channel.read_lines filename
  |> Tools.list_split_all ~f:(Fn.non String.is_empty)
  |> List.map ~f:to_machine_spec

let calc_button_presses machine_spec =
  let [@warning "-8"] [|a1; a2; b1; b2|] = machine_spec.buttons in
  let [@warning "-8"] [|c1; c2|] = machine_spec.prize in
  let det = (a1 *. b2 -. b1 *. a2) in
  (c1 *. b2 -. b1 *. c2) /. det, (a1 *. c2 -. c1 *. a2) /. det 

let apply_to_tuple f (a, b) = f a, f b

let calc_num_tokens button_presses_filter machine_specs =
  machine_specs
  |> List.map ~f:calc_button_presses
  |> List.filter ~f:(fun (a, b) -> Float.is_integer a && Float.is_integer b)
  |> List.map ~f:(apply_to_tuple Float.to_int)
  |> List.filter ~f:button_presses_filter
  |> List.sum (module Int) ~f:(fun (a, b) -> 3 * a + b)

let part_a specs = 
  let button_presses_filter (a, b) = a >= 0 && b >= 0 && a <= 100 && b <= 100 in
  calc_num_tokens button_presses_filter specs

let part_b specs =
  let button_presses_filter (a, b) = a >= 0 && b >= 0 in
  List.map specs ~f:(fun {buttons; prize} -> {buttons; prize = Array.map prize ~f:(fun p -> p +. 10_000_000_000_000.)})
  |> calc_num_tokens button_presses_filter

let solve = solve read_machine_specs part_a part_b

open Core

type equation = {target: int; numbers: int list}

let read_equations filename =
  let read_line line = (
    let [@warning "-8"] target :: numbers = String.split_on_chars line ~on:[':';' ']
    |> List.filter ~f:(Fn.non @@ String.is_empty)
    |> List.map ~f:Int.of_string in
    {target; numbers}
  ) in
  In_channel.read_lines filename
  |> List.map ~f:read_line 

let extend ops target results number should_raise =
  let extend1 op = List.filter_map results ~f:(fun result ->
    let result = op result number in
    match Int.compare result target with 
    | 0 -> if should_raise then raise Exit else Some result
    | x when x < 0 -> Some result
    | _ -> None) in
  List.concat_map ops ~f:extend1

let can_be_calibrated ops {target; numbers} = 
  let [@warning "-8"] first :: numbers = numbers in
  let rec run results = (function
  | [] -> false
  | number :: rest -> 
      try 
        let extended = extend ops target results number (List.is_empty rest) in
        if List.is_empty extended then false else run extended rest 
      with
      | Exit -> true
  ) in
  run [first] numbers

let part_a_ops = [(+); fun x y -> x * y]

let part_b_ops = [(+); (fun x y -> x * y); (fun x y -> Int.of_string [%string "%{x#Int}%{y#Int}"])]

let part_a filename = 
  read_equations filename 
  |> List.filter ~f:(can_be_calibrated part_a_ops)
  |> List.sum (module Int) ~f:(fun e -> e.target)

let part_b filename = 
  read_equations filename 
  |> List.filter ~f:(can_be_calibrated part_b_ops)
  |> List.sum (module Int) ~f:(fun e -> e.target)

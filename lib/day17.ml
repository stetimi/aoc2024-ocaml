open Core

type registers = {
  mutable a: int;
  mutable b: int;
  mutable c: int;
}
[@@deriving ord, show]

type computer = {
  mutable pc: int;
  registers: registers;
}

let parse_colon_sep line ~convert =
let [@warning "-8"] [_; value] = String.split line ~on:':' in
let value = String.drop_prefix value 1 in
convert value

let read_input filename =
  let lines = In_channel.read_lines filename |> List.to_array in
  let parse_register index = parse_colon_sep lines.(index) ~convert:Int.of_string in
  let computer = {
    pc = 0;
    registers = {a = parse_register 0; b = parse_register 1; c = parse_register 2};
  } in
  let program = parse_colon_sep lines.(4) ~convert:(String.split ~on:',') 
  |> List.map ~f:Int.of_string
  |> List.to_array in
  computer, program

let combo_operand registers operand = match operand with 
| _ when 0 <= operand && operand <= 3 -> operand
| 4 -> registers.a
| 5 -> registers.b
| 6 -> registers.c
| _ -> failwith [%string "unhandled operand %{operand#Int}"]

let dv registers operand =
  let num = registers.a in
  let denom = Int.pow 2 (combo_operand registers operand) in
  num / denom

let adv registers operand =
  registers.a <- dv registers operand

let bdv registers operand =
  registers.b <- dv registers operand

let cdv registers operand =
  registers.c <- dv registers operand
    
let bxl registers operand =
  let result = Int.(lxor) registers.b operand in
  registers.b <- result

let jnz computer operand =
  if computer.registers.a <> 0
    then computer.pc <- operand

let bst registers operand =
  let result = Int.rem (combo_operand registers operand) 8 in
  registers.b <- result

let bxc registers =
  registers.b <- Int.(lxor) registers.b registers.c

let out registers out operand =
  let result = Int.rem (combo_operand registers operand) 8 in
  out result

let run1 computer output program = 
  let pc = computer.pc in
  let registers = computer.registers in
  let opcode = program.(pc) in
  let operand = program.(pc + 1) in
  let _ = (match opcode with 
  | 0 -> adv registers operand
  | 1 -> bxl registers operand
  | 2 -> bst registers operand
  | 3 -> jnz computer operand
  | 4 -> bxc registers
  | 5 -> out registers output operand
  | 6 -> bdv registers operand
  | 7 -> cdv registers operand
  | _ -> failwith [%string "Unrecognized opcode %{opcode#Int}"]
  ) in
  if computer.pc = pc 
    then computer.pc <- computer.pc + 2

let run computer program =
  let queue = Queue.create () in
  let program_length = Array.length program in
  let rec go () = (
    if computer.pc < program_length
      then (
        run1 computer (Queue.enqueue queue) program;
        go ()
      )
      else Queue.to_array queue
  ) in 
  go ()

let part_b_run program a =
  let registers = {a; b=0; c=0} in
  let computer = {pc=0; registers} in
  run computer program

let print_part_b_run program a_start a_end =
  for a = a_start to a_end do
    let out = part_b_run program a |> Array.map ~f:Int.to_string in
    print_endline [%string "%{a#Int} - %{String.concat_array ~sep:\"; \" out}}"];
  done

let brute_force () =
  for a = 0o4532316267400000 to 0o4532346277777777 do
    let r = part_b_run [|2; 4; 1; 1; 7; 5; 1; 5; 0; 3; 4; 3; 5; 5; 3; 0|] a in
    if Array.equal (=) r [|2; 4; 1; 1; 7; 5; 1; 5; 0; 3; 4; 3; 5; 5; 3; 0|] then (
      print_endline [%string "DONE: %{a#Int}"];
      raise Exit
    );
    if Int.rem a 100_000 = 0 then 
      let arr = String.concat_array (r |> Array.map ~f:Int.to_string) in
      print_endline [%string "Working: %{a#Int}: %{arr}"];
  done

let part_a filename = 
  let computer, program = read_input filename in
  let result = run computer program in
  String.concat ~sep:"," (result |> Array.to_list |> List.map ~f:Int.to_string)

let part_b _filename = ""
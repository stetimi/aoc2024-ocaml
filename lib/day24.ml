open! Core
open! Tools

type gate_type = Xor | Or | And [@@deriving eq, hash, ord, sexp, show]
type gate = {
  gate_type: gate_type;
  s1: string;
  s2: string;
  out: string
}
[@@deriving eq, hash, ord, sexp, show]

module Gate = struct
  type t = gate [@@deriving eq, hash, ord, sexp, show]
end

type device = {
  signals: (string, bool, String.comparator_witness) Map_intf.Map.t;
  gates: gate list;
}

let parse_signal s =
  let [@warning "-8"] [name; level] = String.split s ~on:':' in
  let level = String.(drop_prefix level 1 = "1") in
  (name, level)

let find_all_starting_with (gates: gate list) (start: char) =
  List.concat_map gates ~f:(fun {gate_type=_; s1; s2; out} -> [s1; s2; out])
  |> List.filter ~f:(fun s -> Char.(String.get s 0 = start))
  |> Set.of_list (module String)

let to_binary (zed_values: bool list) =
  List.fold_left zed_values
    ~init:0L
    ~f:(fun total z -> Int64.(2L * total + (if z then 1L else 0L)))

let read_zed_binary zeds signals =
  let zeds = List.sort zeds ~compare:String.compare in
  let zed_values = List.rev_map zeds ~f:(Map.find_exn signals) in
  to_binary zed_values

let apply_gate_type s1 s2 = function
| Xor ->  Bool.(s1 <> s2)
| Or -> s1 || s2
| And -> s1 && s2

let apply_gate signals gate = 
  let update = Option.(
    Map.find signals gate.s1 >>= fun s1 ->
    Map.find signals gate.s2 >>= fun s2 ->
    return @@ apply_gate_type s1 s2 gate.gate_type
  ) in
  Option.value_map update ~default:signals ~f:(fun s ->
    Map.set signals ~key:gate.out ~data:s
  )

let is_done signals zeds =
  Set.is_subset zeds ~of_:(Map.key_set signals)

let apply_gates signals gates =
  List.fold_left gates ~init:signals ~f:apply_gate

let parse_gate s = 
  let words = String.split s ~on:' ' |> List.to_array in
  let s1 = words.(0) in
  let s2 = words.(2) in
  let gate_type = match words.(1) with | "XOR" -> Xor | "OR" -> Or | "AND" -> And | _ -> failwith "bug" in
  let out = words.(4) in
  {gate_type; s1; s2; out}

let normalize gate =
  if String.compare gate.s1 gate.s2 > 0
    then {s1=gate.s2; s2=gate.s1; gate_type=gate.gate_type; out=gate.out}
    else gate

let read_inputs filename =
  let [@warning "-8"] (signal_strs, _ :: gate_strs) = In_channel.read_lines filename
  |> List.split_while ~f:(Fn.non String.is_empty) in
  let signals = List.map signal_strs ~f:parse_signal |> Map.of_alist_exn (module String) in
  let gates = List.map gate_strs ~f:(parse_gate >> normalize) in
  {signals; gates}

let run signals gates =
  let rec go signals =
    let signals' = apply_gates signals gates in
    if Map.length signals' = Map.length signals 
      then signals'
      else go signals' in
  go signals

let mk_signals bits (x: int64) (y: int64) =
  let to_key letter n = 
    if n <= 9 then [%string "%{letter}0%{n#Int}"] else [%string "%{letter}%{n#Int}"] in
  let (signals, _, _) = List.range 0 bits 
  |> List.fold_left
        ~init:(Map.empty (module String), x, y)
        ~f:(fun (signals, x, y) n -> 
              let add letter value signals = Map.add_exn signals ~key:(to_key letter n) ~data:value in
              let signals = add "x" Int64.(bit_and x 1L = 1L) signals |> add "y" Int64.(bit_and y 1L = 1L) in
              Int64.(signals, x lsr 1, y lsr 1)
            ) in
  signals

let part_a filename = 
  let {signals; gates} = read_inputs filename in
  let signals = run signals gates in 
  let zeds = find_all_starting_with gates 'z' |> Set.to_list in
  read_zed_binary zeds signals

let part_b _filename = 
  0L (* done by hand *)

let has_input name gate = String.(gate.s1 = name || gate.s2 = name)

let has_inputs n1 n2 gate = has_input n1 gate && has_input n2 gate

let matches_gate n1 n2 gate_type gate = has_inputs n1 n2 gate && equal_gate_type gate.gate_type gate_type
 
let y_in x_in = [%string "y%{String.drop_prefix x_in 1}"]

let zed x_in = [%string "z%{String.drop_prefix x_in 1}"]

let find_gates_with_out (gates: gate list) out = List.filter gates ~f:(fun g -> String.(g.out = out))

let find_gates_with_in (gates: gate list) _in = List.filter gates ~f:(has_input _in)

type half_adder = {out: string; carry_out: string}
[@@deriving show]

type adder = {out: string; carry_in: string; carry_out: string}
[@@deriving show]

type analysis =
| Bad_output of gate * string
| Bad_input of string
| Adder of adder 
| Half_adder of half_adder
[@@deriving show]

let checked_full_adder all_gates gates x_in = 
  let untrack_gate = Hash_set.strict_remove_exn all_gates in
  let y_in = y_in x_in in
  let zed = zed x_in in
  let xor1_gate = List.filter gates ~f:(matches_gate x_in y_in Xor) |> single_exn in
  let and1_gate = List.filter gates ~f:(matches_gate x_in y_in And) |> single_exn in
  let adder xor2_gate =   
    let carry_in = if String.(xor2_gate.s1 = xor1_gate.out) then xor2_gate.s2 else xor2_gate.s1 in
    let and2_gate = List.filter gates ~f:(matches_gate xor1_gate.out carry_in And) |> single_exn in
    let or_gate = List.filter gates ~f:(matches_gate and1_gate.out and2_gate.out Or) |> single_exn in
    let carry_out = or_gate.out in
    List.iter [xor1_gate; xor2_gate; and1_gate; and2_gate; or_gate] ~f:untrack_gate;
    Adder {out=xor2_gate.out; carry_in; carry_out} in
  match List.filter gates ~f:(fun g -> has_input xor1_gate.out g && equal_gate_type Xor g.gate_type) with
  | [xor2_gate] when String.(xor2_gate.out = zed) -> adder xor2_gate 
  | [xor2_gate] -> Bad_output (xor2_gate, zed)
  | [] -> Bad_output (xor1_gate, zed)
  | _ -> failwith "unhandled"

let checked_adder all_gates gates x_in =
  if String.(x_in = "x00") 
    then 
      let related_gates = find_gates_with_in gates x_in in
      let xor_gate = List.filter related_gates ~f:(fun g -> equal_gate_type Xor g.gate_type) |> single_exn in
      let and_gate = List.filter related_gates ~f:(fun g -> equal_gate_type And g.gate_type) |> single_exn in
      Hash_set.strict_remove_exn all_gates xor_gate;
      Hash_set.strict_remove_exn all_gates and_gate;
      Half_adder {out=xor_gate.out; carry_out=and_gate.out}
    else checked_full_adder all_gates gates x_in

let checked_adders gates =
  let x_ins = List.filter_map gates ~f:(fun g -> Option.some_if (String.is_prefix ~prefix:"x" g.s1) g.s1) 
  |> Set.of_list (module String)
  |> Set.to_list in
  let all_gates = Hash_set.create (module Gate) in
  List.iter gates ~f:(Hash_set.strict_add_exn all_gates);
  let adders = List.map x_ins ~f:(checked_adder all_gates gates) in
  let unused_gates = Hash_set.to_list all_gates in
  (adders, unused_gates)
  
let swap_outs o1 o2 (gates: gate list) =
  let swap (g: gate) =
    if String.(g.out = o1) then {g with out = o2}
    else if String.(g.out = o2) then {g with out = o1}
    else g in
  List.map gates ~f:swap

let solved gates =
  gates (* 24 *)
  |> swap_outs "fkp" "z06" (* 21 *)
  |> swap_outs "ngr" "z11" (* 16*)
  |> swap_outs "krj" "bpt" (* 11*)
  |> swap_outs "z31" "mfm" 


(*
221 gates
Half adder has 2 (z00)
Z44 has carry_out = Z45

Z38, carry_in = ntr, carry_out = hsp
{gate_type = Or; s1 = "krj"; s2 = "tdv"; out = "hsp"};
{gate_type = Xor; s1 = "bpt"; s2 = "ntr"; out = "z38"}

{gate_type = Xor; s1 = "x38"; s2 = "y38"; out = "krj"};
{gate_type = And; s1 = "x38"; s2 = "y38"; out = "bpt"};
{gate_type = And; s1 = "bpt"; s2 = "ntr"; out = "tdv"}

Z31, carry_in = tpf, carry_out = brs
{gate_type = Xor; s1 = "x31"; s2 = "y31"; out = "mgq"}
{gate_type = And; s1 = "x31"; s2 = "y31"; out = "z31"}  X
{gate_type = Xor; s1 = "mgq"; s2 = "tpf"; out = "mfm"}

Z12, carry_in = knj, carry_out = vjn
{gate_type = Xor; s1 = "x12"; s2 = "y12"; out = "ctw"
{gate_type = And; s1 = "x12"; s2 = "y12"; out = "ptj"};
{gate_type = And; s1 = "ctw"; s2 = "knj"; out = "vqm"}


Z44
{gate_type = Xor; s1 = "x44"; s2 = "y44"; out = "gcd"};
{gate_type = And; s1 = "x44"; s2 = "y44"; out = "gvk"}
{gate_type = Xor; s1 = "gcd"; s2 = "mdn"; out = "z44"}
{gate_type = And; s1 = "gcd"; s2 = "mdn"; out = "jkn"}
{gate_type = Or; s1 = "gvk"; s2 = "jkn"; out = "z45"}

Z00
{gate_type = Xor; s1 = "x00"; s2 = "y00"; out = "z00"};
{gate_type = And; s1 = "x00"; s2 = "y00"; out = "skt"}



*)
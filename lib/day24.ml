open Core

type gate_type = Xor | Or | And [@@deriving eq, show]
type gate = {
  gate_type: gate_type;
  s1: string;
  s2: string;
  out: string
}
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

let read_inputs filename =
  let [@warning "-8"] (signal_strs, _ :: gate_strs) = In_channel.read_lines filename
  |> List.split_while ~f:(Fn.non String.is_empty) in
  let signals = List.map signal_strs ~f:parse_signal |> Map.of_alist_exn (module String) in
  let gates = List.map gate_strs ~f:parse_gate in
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
    if n <= 9 then [%string "%{letter}0%{n#Int}"] else [%string "%{letter}{n#Int}"] in
  let (signals, _, _) = List.range 0 bits 
  |> List.fold_left
        ~init:(Map.empty (module String), x, y)
        ~f:(fun (signals, x, y) n -> 
              let add letter value signals = Map.add_exn signals ~key:(to_key letter n) ~data:value in
              let signals = add "x" Int64.(bit_and x 1L = 1L) signals |> add "y" Int64.(bit_and y 1L = 1L) in
              Int64.(signals, x lsr 1, y lsr 1)
            ) in
  signals

let add gates bits x y =
  let signals = mk_signals bits x y in
  run signals gates 

module G = Graph.Imperative.Digraph.Concrete(String)
module BFS = Graph.Traverse.Bfs(G)

let mk_inputs_graph gates =
  let g = G.create () in
  List.iter gates ~f:(fun gate ->
    G.add_vertex g gate.s1;
    G.add_vertex g gate.s2;
    G.add_vertex g gate.out;
    G.add_edge g gate.out gate.s1;
    G.add_edge g gate.out gate.s2;
  );
  g
let find_recursive_inputs =
  BFS.fold_component (fun v acc -> v :: acc) [] 

let find_faulty_outputs find_recursive_inputs gates zeds signals =
  let unset_zeds = Set.diff (Map.key_set signals) zeds |> Set.to_list in
  let inputs_for_unset_zeds = List.concat_map unset_zeds ~f:find_recursive_inputs |> Set.of_list (module String) in
  let outputs_for_unset_zed_inputs = List.filter gates ~f:(fun g -> Set.mem inputs_for_unset_zeds g.out) |> List.map ~f:(fun g -> g.out) in
  List.concat [outputs_for_unset_zed_inputs; unset_zeds]
    
let part_a filename = 
  let {signals; gates} = read_inputs filename in
  let signals = run signals gates in 
  let zeds = find_all_starting_with gates 'z' |> Set.to_list in
  read_zed_binary zeds signals

let filename = "test/test_inputs/day24.txt"

let part_b filename = 
  let {signals=_; gates} = read_inputs filename in
  let bits = find_all_starting_with gates 'x'
  |> Set.map (module Int) ~f:(fun z -> String.drop_prefix z 1 |> Int.of_string)
  |> Set.max_elt_exn in
  let inputs_graph = mk_inputs_graph gates in
  let zeds = find_all_starting_with gates 'z' in
  let find_recursive_inputs = find_recursive_inputs inputs_graph in
  let faulty = find_faulty_outputs find_recursive_inputs gates zeds (add gates bits 0L 0L) in
  List.length faulty |> Int64.of_int
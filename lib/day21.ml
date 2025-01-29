open! Core
open! Tools

let numeric = "789456123 0A" |> String.to_array
let directional = " ^A<v>" |> String.to_array

let read_codes = In_channel.read_lines

let to_direction = function
| '<' -> -1
| '>' -> 1
| '^' -> -3
| 'v' -> 3
| _ -> failwith "unhandled"

let walk keypad x y path =
  path
  |> Sequence.map ~f:to_direction
  |> Sequence.folding_map ~init:(y * 3 + x) ~f:(fun i d ->
    let i = Array.normalize keypad (i + d) mod (Array.length keypad) in
    i, keypad.(i)
  )

let divmod x y = x / y, x mod y

let repeat len ch = Array.create ~len ch |> String.of_array

let paths_between keypad start finish =
  let index v = Array.findi_exn keypad ~f:(fun _ k -> Char.(k = v)) |> fst in
  let y1, x1 = divmod (index start) 3 in
  let y2, x2 = divmod (index finish) 3 in
  let hor_ch = if x2 > x1 then '>' else '<' in
  let hor = repeat (Int.abs (x2 - x1)) hor_ch in
  let ver_ch = if y2 > y1 then 'v' else '^' in
  let ver = repeat (Int.abs (y2 - y1)) ver_ch in
  let falls_into_gap path = 
    let walked = walk keypad x1 y1 (Array.to_sequence @@ String.to_array path) in
    Sequence.mem walked ' ' ~equal:(Char.equal) in
  List.remove_consecutive_duplicates [hor ^ ver; ver ^ hor] ~equal:String.equal
  |> List.filter ~f:(Fn.non falls_into_gap)
  |> List.map ~f:(fun path -> path ^ "A")

module CostBetweenKey = struct
  type t = char * char * int [@@deriving hash, ord, sexp]
end

module CostKey = struct
  type t = string * int [@@deriving hash, ord, sexp]
end

type memos = {
  cost_between_cache: (CostBetweenKey.t, int) Hashtbl.t;
  cost_cache: (CostKey.t, int) Hashtbl.t;
}

let rec cost_between memos keypad start finish links = 
  if links <= 0 then 1 else
  match Hashtbl.find memos.cost_between_cache (start, finish, links) with 
  | None ->
    let paths = paths_between keypad start finish in
    let costs = List.map paths ~f:(fun path -> cost memos directional path (links - 1)) in
    let cost_between = List.min_elt costs ~compare:Int.compare |> Option.value_exn in
    Hashtbl.set memos.cost_between_cache ~key:(start, finish, links) ~data:cost_between;
    cost_between
  | Some costs -> costs
and cost memos keypad keys links =
  match Hashtbl.find memos.cost_cache (keys, links) with 
  | None ->
      let paths = zip_nexts @@ String.to_list ("A" ^ keys) in
      let cost_between_keys (a, b) = cost_between memos keypad a b links in
      let cost = List.sum (module Int) paths ~f:cost_between_keys in
      Hashtbl.set memos.cost_cache ~key:(keys, links) ~data:cost;
      cost
  | Some cost -> cost

let numeric_part code =
  Int.of_string @@ String.drop_suffix code 1

let complexity robots code =
  let memos = {
    cost_between_cache = Hashtbl.create (module CostBetweenKey);
    cost_cache = Hashtbl.create (module CostKey);
  } in
  cost memos numeric code (robots + 1) * numeric_part code

let part_a filename =
  let codes = read_codes filename in
  List.sum (module Int) codes ~f:(complexity 2)

let part_b filename =
  let codes = read_codes filename in
  List.sum (module Int) codes ~f:(complexity 25)
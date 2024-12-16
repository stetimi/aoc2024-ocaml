open Core

type stone = int64
type stones_map = (stone, int64, Int64.comparator_witness) Map.t
let no_stones = Map.empty (module Int64)

let parse_stones text: stones_map =
  let stones = text
  |> String.split ~on:' '
  |> List.map ~f:(Int64.of_string) in
  List.fold_left stones ~init:no_stones ~f:(fun frequencies stone -> 
    Map.update frequencies stone ~f:(function
    | None -> 1L
    | Some x -> Int64.(x + 1L)
    )
  )

let read_stones filename =
  In_channel.read_all filename |> parse_stones

let is_even_number_of_digits n = Int.rem (Int.to_string n |> String.length) 2 = 0

let split_if_even (n: int64): (int64 * int64) option =
  let n = Int64.to_string n in
  let len = String.length n in
  if Int64.(rem (len |> Int64.of_int) 2L = 0L)
    then Some (Int64.of_string @@ String.prefix n (len / 2), Int64.of_string @@ String.suffix n (len / 2))
    else None

let add_stones (frequencies: stones_map) ~stone ~count =
  Map.update frequencies stone ~f:(function
  | None -> count
  | Some n -> Int64.(n + count)
  )
  
let blink_one_stone (frequencies: stones_map) (frequencies': stones_map) (stone: stone): stones_map = 
  let count = Map.find_exn frequencies stone in
  match stone with 
  | 0L -> add_stones frequencies' ~stone:1L ~count
  | _ -> (match split_if_even stone with
    | None -> add_stones frequencies' ~stone:Int64.(stone * 2024L) ~count
    | Some (s1, s2) -> add_stones frequencies' ~stone:s1 ~count |> add_stones ~stone:s2 ~count
    ) 

let blink_one_time (frequencies: stones_map): stones_map = 
  let stones = Map.keys frequencies in
  List.fold_left stones ~init:no_stones ~f:(blink_one_stone frequencies)

let count frequencies: int64 =
  Map.sum (module Int64) frequencies ~f:Fn.id
  
let blink n filename =
  let frequencies = ref @@ read_stones filename in
  for _ = 1 to n do
    frequencies := blink_one_time !frequencies
  done;
  count !frequencies |> Int64.to_int_exn

let part_a = blink 25

let part_b = blink 75
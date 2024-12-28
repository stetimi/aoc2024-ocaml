open Core
open Tools

let read_secret_numbers filename =
  In_channel.read_lines filename |> List.map ~f:Int.of_string

let evolve n =
  let n = ((n * 64) lxor n) mod 16777216 in
  let n = ((n / 32) lxor n) mod 16777216 in
  let n = ((n * 2048) lxor n) mod 16777216 in
  n

let repeat (n: int) (f: 'a -> 'a): 'a -> 'a =
  let rec iter n result =
    if n = 0
      then result
      else iter (n-1) (f result) in
  iter n

let accum_repeat (n: int) (f: 'a -> 'a): 'a -> 'a list =
  let rec iter n (acc: 'a list) result =
    if n = 0
      then acc
      else iter (n-1) (result :: acc) (f result) in
  iter n [] >> List.rev

let changes xs =
  fst @@ List.zip_with_remainder xs (List.tl_exn xs)
  |> List.map ~f:(fun (x,y) -> y - x)

let encode = function
| [n1; n2; n3; n4] -> (n1 + 9) lsl 15 + (n2 + 9) lsl 10 + (n3 + 9) lsl 5 + (n4 + 9)
| _ -> failwith "bug"

let windows (len: int) (sequence: 'a list): 'a list Sequence.t =
  Sequence.unfold 
    ~init:sequence
    ~f:(fun s ->
      let window, tail = List.split_n s len in
      if List.is_empty tail
        then if List.length window = len then Some (window, []) else None
        else Some (window, List.tl_exn s) 
      ) 

let part_a filename = 
  let secret_numbers = read_secret_numbers filename in
  List.sum (module Int) secret_numbers ~f:(repeat 2000 evolve)

type hiding_spot = {
  sequence: int;
  price: int;
}

let hiding_spots (secret_number: int): hiding_spot list =
  let prices = accum_repeat 2000 evolve secret_number |> List.map ~f:(fun x -> x mod 10) in
  let change_windows = prices |> changes |> windows 4 |> Sequence.to_list in
  let prices = List.drop prices 4 in
  let already_seen = Hash_set.create (module Int) in
  List.zip_exn prices change_windows 
  |> List.filter_map ~f:(fun (price,sequence) -> 
    let sequence = encode sequence in
    if Hash_set.mem already_seen sequence
      then None
      else (
        Hash_set.add already_seen sequence; 
        Some ({sequence=sequence; price})
      )
  ) 

let part_b filename = 
  let all_hiding_spots = read_secret_numbers filename
  |> List.concat_map ~f:hiding_spots in
  let hiding_spot_total_prices = List.fold_left 
    all_hiding_spots
    ~init:(Map.empty (module Int))
    ~f:(fun m {sequence; price} -> (
      Map.update m sequence ~f:(function None -> price | Some t -> price + t)
    )) in
  Map.to_alist hiding_spot_total_prices 
  |> List.max_elt ~compare:(on snd Int.compare) 
  |> Option.value_exn
  |> snd
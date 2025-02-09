open! Core
open Timings

type schema = int array

type schemas = {
  locks: schema list;
  keys: schema list;
}

let parse_schema (lines: string list) =
  let sizes = Array.create ~len:5 (-1) in
  List.iter lines ~f:(
    String.iteri ~f:Char.(fun i ch -> if ch = '#' then sizes.(i) <- sizes.(i) + 1)
  );
  sizes

let read_raw filename =
  In_channel.read_lines filename
  |> List.group ~break:(fun l1 _ ->String.is_empty l1)
  |> List.map ~f:(List.filter ~f:(Fn.non String.is_empty)) 

let read_schemas filename =
  let locks = ref [] in
  let keys = ref [] in
  let raw = read_raw filename in
  List.iter raw ~f:(fun lines ->
    let schemas = if String.(List.hd_exn lines = "#####") then locks else keys in
    let schema = parse_schema lines in
    schemas := schema :: !schemas
  );
  {locks = !locks; keys = !keys}

let key_fits_lock lock key =
  let rec check i =
    if i = 5 then true
    else if lock.(i) + key.(i) > 5 then false
    else check (i + 1) in
  check 0

let count_keys_fitting_lock lock = List.count ~f:(key_fits_lock lock)

let part_a filename = 
  let {locks; keys} = read_schemas filename in
  List.sum (module Int) locks ~f:(fun lock -> count_keys_fitting_lock lock keys) 

let part_b _filename = 0

let solve = solve Fn.id part_a part_b

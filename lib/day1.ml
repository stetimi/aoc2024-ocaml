open Core

let parse_ints s =
  let parts = String.split_on_chars s ~on:[' '] |> List.filter ~f:(String.is_empty |> Fun.negate) in
  assert (List.length parts = 2);
  let x = List.hd_exn parts in
  let y = List.last_exn parts in
  [Int.of_string x; Int.of_string y]

let read_lists lines =
  let [@warning "-8"] [first; second] = List.map lines ~f:parse_ints |> List.transpose_exn in
  (first, second)

let read_input filename = In_channel.read_lines filename |> read_lists

let part_a filename =
  let (first, second) = read_input filename in
  let sort = List.sort ~compare:Int.compare in
  let diffs = List.zip_exn (sort first) (sort second) |> List.map ~f:(fun (x, y) -> Int.abs (x - y)) in
  List.sum (module Int) diffs ~f:Fun.id

let part_b filename =
  let (first, second) = read_input filename in
  let second = Bag.of_list second in
  let counts = List.map first ~f:(fun x1 -> x1 * Bag.count second ~f:(fun x2 -> x1 = x2)) in
  List.sum (module Int) counts ~f:Fun.id

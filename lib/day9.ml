open Core

type file = {id: int; length: int} [@@deriving show]

type disk_sector =
| File of file
| Space of int
[@@deriving show]

type disk = disk_sector list
[@@deriving show]

type space_ptr = {index: int; length: int}

let print_spaces spaces =
  print_endline @@  Tools.map_printer Int.to_string (Tools.list_printer @@ Int.to_string) spaces

let print_disk_byte = function
| (-1) -> "."
| n -> Int.to_string n

let print_disk disk = 
  print_endline (disk |> List.of_array |> Fn.flip List.take 45 |> List.map ~f:print_disk_byte |> String.concat)

let add_space m space_ptr =
  Map.update m space_ptr.length ~f:(function
  | None -> [space_ptr.index] 
  | Some elts -> space_ptr.index :: elts
  )

  let rec find_spaces spaces length max_length =
  if length > max_length then None
  else
    match Map.find spaces length with
    | None -> find_spaces spaces (length + 1) max_length
    | Some space_ptrs -> (match space_ptrs with
      | [] -> find_spaces spaces (length + 1) max_length
      | xs -> Some (length, xs)
    )

let remove_best_space_index spaces min_length =
  let max_length = fst @@ Map.max_elt_exn spaces in
  match find_spaces spaces min_length max_length with
  | None -> None, spaces
  | Some (_, []) -> failwith "No values, unhandled"
  | Some (len, index :: rest) -> 
      Some index, Map.set spaces ~key:len ~data:rest

let to_disk_rev (s: string) =
  String.to_array s
  |> Array.foldi ~init:[] ~f:(fun index disk ch ->
    let length = (Char.to_int ch) - 48 in
    let sector = if Int.rem index 2 = 0
      then let id = index / 2 in File {id; length}
      else Space length in
    sector::disk
  )

let to_array disk_rev =
  let disk = List.rev disk_rev in
  let disk_array = Array.init 250_000 ~f:(Fun.const (-1)) in
  let head = ref 0 in
  let copy value length = (
    for i = 0 to (length - 1) do
      Array.set disk_array (i + !head) value
    done;
    head := !head + length
  ) in
  let spaces = List.fold disk ~init:(Map.empty (module Int)) ~f:(fun spaces -> function
  | Space 0 -> spaces
  | Space length -> 
      let spaces = add_space spaces {index = !head; length} in
      copy (-1) length;
      spaces
  | File {id; length} -> 
      copy id length;
      spaces
  ) in
  let spaces = Map.map spaces ~f:List.rev in
  (* print_disk disk_array; *)
  disk_array, spaces

let defragment_fully disk =
  let i_start = ref 0 in
  let i_end = ref @@ Array.length disk - 1 in
  let get ref = Array.get disk !ref in
  let set ref = Array.set disk !ref in
  while !i_start < !i_end do
    if Array.get disk !i_start = -1 
    then (
      let moved_char =  get i_end in
      if moved_char <> -1 then (
        set i_start moved_char;
        set i_end (-1);
        Ref.replace i_start (Int.succ);
      );
      Ref.replace i_end (Int.pred); 
    )
    else Ref.replace i_start (Int.succ);
  done

let defragment_best_efforts disk_rev (disk: int array) spaces: unit =
  let copy id index len = (
    (* print_endline [%string "Copying id: %{id#Int} to %{index#Int}, length %{len#Int}"]; *)
    for i = index to index + len - 1 do
      (* print_endline [%string "Setting %{i#Int} to %{id#Int}"]; *)
      Array.set disk i id
    done) in
  let _ = List.fold_left disk_rev ~init:spaces ~f:(fun spaces sector -> match sector with
    | Space _ -> spaces
    | File {id; length} -> 
      let space_index, spaces = remove_best_space_index spaces length in 
      Option.iter space_index ~f:(fun space_index -> 
        copy id space_index length;
        (* copy (-1) 0 length; *)
      );
      (* print_disk disk; *)
      spaces
  ) in
  ()

let checksum disk =
  Array.to_sequence disk
  |> Sequence.take_while ~f:(fun n -> n <> -1)
  |> Sequence.mapi ~f:(fun i n -> i * n)
  |> Sequence.sum (module Int) ~f:Fun.id

let part_a filename = 
  let disk_map = In_channel.read_all filename in
  let disk, _ = to_disk_rev disk_map |> to_array in
  defragment_fully disk;
  checksum disk

let part_b filename = 
  let disk_map = In_channel.read_all filename in
  let disk_rev = to_disk_rev disk_map in
  (* print_endline @@ show_disk disk_rev; *)
  let disk, spaces = to_array disk_rev in
  (* print_endline ""; *)
  (* print_spaces spaces; *)
  defragment_best_efforts disk_rev disk spaces;
  checksum disk

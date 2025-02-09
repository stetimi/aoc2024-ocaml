open! Core
open Tools
open Timings

let space = -1

type disk = int array

let to_disk (disk_map: string) ~(on_space: int -> int -> unit): disk =
  let disk_map = String.to_array disk_map in
  let array_index = ref 0 in
  Array.concat_mapi disk_map
    ~f:(fun i ch ->
      let len = Char.to_int ch - 48 in
      let arr = if i % 2 = 0
        then let id = i / 2 in Array.create ~len id
        else (
          on_space !array_index len;
          Array.create ~len space
        ) in
        array_index := !array_index + len;
      arr
    )

let defragment_by_byte (disk: disk): int =
  let read_ptr = ref (Array.length disk - 1) in
  let write_ptr = ref 0 in
  while !write_ptr < !read_ptr do
    while disk.(!write_ptr) <> space do
      incr write_ptr
    done;
    while disk.(!read_ptr) = space do
      decr read_ptr
    done;
    disk.(!write_ptr) <- disk.(!read_ptr);
    incr write_ptr;
    decr read_ptr;
  done;
  !write_ptr

let pop_space_that_fits len index spaces_by_len =
  let pop_first_index spaces len =
    let spaces = Set.remove_index spaces 0 in
    if Set.is_empty spaces 
        then Hashtbl.remove spaces_by_len len
        else
          Hashtbl.set spaces_by_len ~key:len ~data:spaces in
  let entry_with_min_id = Hashtbl.filter_keys spaces_by_len ~f:(fun l -> l >= len)
  |> Hashtbl.to_alist 
  |> List.min_elt ~compare:(fun (_, i1) (_, i2) -> on Set.min_elt_exn Int.compare i1 i2) in
  match entry_with_min_id with
  | None -> None
  | Some (spaces_len, space_indices) ->
    let min_space_index = Set.min_elt_exn space_indices in
    if min_space_index < index 
      then (
        pop_first_index space_indices spaces_len;
        Some (spaces_len, min_space_index)
      ) else None

let add_space len index spaces_by_len =
  Hashtbl.update spaces_by_len len ~f:(function
    | None -> Set.singleton (module Int) index
    | Some spaces -> Set.add spaces index)

let defragment_by_sector (disk: disk) spaces_by_len: unit =
  let lowest_block_copied = ref Int.max_value in
  let read_ptr = ref (Array.length disk - 1) in
  let read_block () =
    while disk.(!read_ptr) = space || disk.(!read_ptr) >= !lowest_block_copied do
      decr read_ptr;
    done;
    let first = disk.(!read_ptr) in
    let len = ref 0 in
    while !read_ptr >= 0 && disk.(!read_ptr) = first do
      decr read_ptr;
      incr len;
    done;
    lowest_block_copied := first;
    (first, !len) in
  while !read_ptr >= 0 do
    let (block_id, block_len) = read_block () in
    match pop_space_that_fits block_len (!read_ptr) spaces_by_len with 
    | None -> ( 
        ()
      )
    | Some (len, index) -> (
        for i = 0 to block_len - 1 do
          disk.(index + i) <- block_id;
          disk.(!read_ptr + i + 1) <- space;
        done;
        if len > block_len then
          add_space (len - block_len) (index + block_len) spaces_by_len;
        ()
      )
  done

let checksum (disk: disk) (limit: int): int =
  let sum = ref 0 in
  for i = 0 to limit do
    let id = disk.(i) in
    if id <> space then sum := !sum + i * id;
  done;
  !sum

let part_a disk_map = 
  let disk = to_disk disk_map ~on_space:(fun _ _ -> ()) in
  let endi = defragment_by_byte disk in
  checksum disk endi

let part_b disk_map = 
  let spaces_by_len = Hashtbl.create (module Int) in
  let on_space index len = Hashtbl.change spaces_by_len len ~f:(fun ids ->
    if len = 0 then None
    else match ids with
      | None -> Some (Set.singleton (module Int) index)
      | Some ids -> Some (Set.add ids index)) in
  let disk = to_disk disk_map ~on_space in
  defragment_by_sector disk spaces_by_len;
  checksum disk (Array.length disk - 1)

let solve = solve In_channel.read_all part_a part_b

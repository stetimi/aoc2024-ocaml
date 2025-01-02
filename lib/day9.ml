open Core

let space = -1

type file = {id: int; len: int} [@@deriving show]

type sector =
| Space of int
| File of file [@@deriving show]

type disk = sector array

(* DEBUGGING *)
let show_sector = function
| Space len -> String.init len ~f:(Fn.const '.') 
| File {id; len} -> String.init len ~f:(Fn.const (Char.of_int_exn (48+id)))

let show_disk disk =
  String.concat_array (Array.map disk ~f:show_sector) ~sep:"" 

let dump_disk filename disk =
  let sector_to_ints = function
  | Space n -> Array.init n ~f:(Fn.const (-1))
  | File {id; len} -> Array.init len ~f:(Fn.const id) in
  let data = Array.concat_map disk ~f:sector_to_ints |> Array.map ~f:Int.to_string |> String.concat_array ~sep:"," in
  Out_channel.write_all filename ~data
(* END DEBUGGING *)

let to_disk (disk_map: char array): disk =
  Array.mapi disk_map
    ~f:(fun i ch ->
      let len = Char.to_int ch - 48 in
      if i % 2 = 0
        then let id = i / 2 in File {id; len}
        else Space len
    )

let copy_back disk space_len end_i =
  let rec go () = match disk.(!end_i) with
  | Space _ -> (
      end_i := !end_i - 1;
      go ()
  )
  | File {id; len} -> (
      match space_len with
      | _ when space_len < len -> (
            disk.(!end_i) <- File {id; len=len-space_len};
            (id, space_len)
          )
      | _ -> (
            end_i := (!end_i) - 1;
            (id, len)
          )
  ) in
  go ()

let defragment_by_byte (disk: disk): disk =
  let start_i = ref 0 in
  let end_i = ref (Array.length disk - 1) in
  let defragged = CCVector.create () in
  let copy () = match disk.(!start_i) with
  | File f -> (
      start_i := !start_i + 1;
      CCVector.push defragged (File f)
  )
  | Space space_len -> (
      let (id, file_len) = copy_back disk space_len end_i in
      if space_len > file_len
        then disk.(!start_i) <- Space (space_len - file_len)
        else start_i := !start_i + 1;
      CCVector.push defragged (File {id; len=file_len})
  ) in
  let rec run () =
    if !start_i < !end_i
      then (
        copy ();
        run ()
      ) else (
        CCVector.push defragged disk.(!start_i)
      ) in  
  run ();
  CCVector.to_array defragged

let maps (disk: disk) =
  Array.foldi disk ~init:(Map.empty (module Int), []) ~f:(fun i (spaces_map, indexed_files) sector ->
    match sector with
    | Space len when len <> 0 -> Map.add_multi spaces_map ~key:len ~data:i, indexed_files
    | Space _ -> spaces_map, indexed_files
    | File f -> spaces_map, (i, f) :: indexed_files
  )
  |> Tuple2.map_fst ~f:(Map.map ~f:List.rev)

let defragment_by_sector (disk: disk) =
  let _spaces_map, _indexed_files = maps disk in
  disk

let sum_range index len =
  let sum n = n * (n - 1) / 2 in
  sum (index + len) - sum index

let checksum (disk: sector list): int =
  let sector_checksum acc index = function
  | Space len -> (acc, index + len)
  | File {id; len} -> (acc + id * (sum_range index len), index + len) in
  List.fold_left disk ~init:(0,0) ~f:(fun (acc, index) -> sector_checksum acc index) |> fst

let part_a filename = 
  let disk = In_channel.read_all filename |> String.to_array |> to_disk in
  let defragged = defragment_by_byte disk in
  checksum (Array.to_list defragged)

let disk () = In_channel.read_all "test/test_inputs/day9.txt" |> String.to_array |> to_disk

let part_b filename = 
  let disk = In_channel.read_all filename |> String.to_array |> to_disk in
  let _defragged = defragment_by_sector disk in
  2858
  (* checksum (Array.to_list defragged) *)


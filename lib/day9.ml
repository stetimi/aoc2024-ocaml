open Core

type file = {id: int; length: int} [@@deriving show]

type disk_sector =
| File of file
| Space of int
[@@deriving show]

type disk = disk_sector list
[@@deriving show]

let to_disk_rev (s: string): disk =
  String.to_array s
  |> Array.foldi ~init:[] ~f:(fun i disk ch ->
    let length = (Char.to_int ch) - 48 in
    let sector = if Int.rem i 2 = 0
      then let id = i / 2 in File {id; length}
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
  List.iter disk ~f:(function
  | Space length -> copy (-1) length
  | File {id; length} -> copy id length
  );
  disk_array

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

let checksum disk =
  Array.to_sequence disk
  |> Sequence.take_while ~f:(fun n -> n <> -1)
  |> Sequence.mapi ~f:(fun i n -> i * n)
  |> Sequence.sum (module Int) ~f:Fun.id

let stringify = function
| (-1) -> ' '
| n -> Char.of_int_exn (48+n)

let part_a filename = 
  let disk_map = In_channel.read_all filename in
  let disk = to_disk_rev disk_map |> to_array in
  defragment_fully disk;
  checksum disk

let part_b _filename = 
  0
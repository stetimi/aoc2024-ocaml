open Core

let part_a filename = 
  let disk_map = In_channel.read_all filename in
  let _disk = Array.init (String.length disk_map) ~f:(Fun.const '.') in
  0

let part_b filename = 
  let _disk_map = In_channel.read_all filename in
  0

open! Core
open! Tools

type 'a timed_result = {
  result: 'a;
  nanos: Int63.t
}

type ('a, 'b) timed_solution = {
  part_a: 'a timed_result;
  part_b: 'b timed_result;
  shared_nanos: Int63.t;
} 

let timed (f: unit -> 'a): 'a timed_result =
  let time_ns= Time_ns.now >> Time_ns.to_int63_ns_since_epoch in
  let start = time_ns () in
  let result = f () in
  let finish = time_ns () in
  {result; nanos=Int63.(finish - start)}

let solve shared part_a part_b filename =
  let shared_timing = timed (fun () -> shared filename) in
  let part_a = timed @@ fun () -> part_a (shared_timing.result) in
  let part_b = timed @@ fun () -> part_b (shared_timing.result) in
  {part_a; part_b; shared_nanos=shared_timing.nanos}
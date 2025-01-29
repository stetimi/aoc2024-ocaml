open! Core

module type HeapType = sig
  type t [@@deriving ord]
end

module PriorityQueue(V: HeapType) = struct
  type element = {priority: int; value: V.t} [@@deriving ord]
  module Heap = CCHeap.Make_from_compare(struct type t = element [@@deriving ord] end)

  type t = {
    heap: Heap.t ref
  }

  let create (): t = {heap = ref Heap.empty}

  let insert (pq: t) (priority: int) (value: V.t): unit = pq.heap := Heap.insert {priority; value} !(pq.heap)

  let take (pq: t): V.t option = 
    Heap.take !(pq.heap) |> Option.map ~f:(fun (h, e) -> pq.heap := h; e.value)

  let find_min (pq: t): V.t option = Heap.find_min !(pq.heap) |> Option.map ~f:(fun e -> e.value)
end

open! Core

module Dijkstra(V: sig type t [@@deriving hash, sexp, ord] end) = struct
  let compare_vertex = V.compare

  module PriorityNode = struct
    type t = int * V.t [@@deriving hash, sexp, ord]
  end
  module PrioritySet = Set.Make(PriorityNode)
  module VertexSet = Set.Make(V)

  let all_shortest_paths 
    ~(vertices: V.t list)
    ~(neighbours: V.t -> V.t list) 
    ~(source: V.t) 
    ~(is_end: V.t -> bool)
    ~(weight: V.t -> V.t -> int) = 
    let dist = Hashtbl.create (module V) in
    let prevs = Hashtbl.create (module V) in
    List.iter vertices ~f:Hashtbl.(fun v -> 
      add_exn dist ~key:v ~data:Int.max_value_30_bits;
      add_exn prevs ~key:v ~data:VertexSet.empty;
    );
    let q = ref @@ PrioritySet.singleton (0, source) in
    Hashtbl.set dist ~key:source ~data:0; 
    let dist_for v = Hashtbl.find_exn dist v in
    while not (Set.is_empty !q) do
      let (d, u) = Set.min_elt_exn !q in
      q := Set.remove !q (d, u);
      if not @@ is_end u then
        List.iter
          (neighbours u)
          ~f:(fun v -> 
            let w = weight u v in
            let new_dist = dist_for u + w in
            let dist_v = dist_for v in
            if new_dist > dist_v then 
              ()
            else (
              q := Set.add !q (new_dist, v);
              let new_prevs = if new_dist = dist_v 
                then fun vs -> Set.add vs u
                else Fn.const @@ VertexSet.singleton u in
              Hashtbl.(
                set dist ~key:v ~data:new_dist; 
                update prevs v ~f:(fun ovs -> new_prevs @@ Option.value_exn ovs);
              );
            )
          );
    done;
    dist, prevs
end

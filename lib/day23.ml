open Core
open Graph
open Tools

type string_set = (string, String.comparator_witness) Set_intf.Set.t
type string_map = (string, string_set, String.comparator_witness) Map_intf.Map.t

module StringTriple = struct
  type t = string * string * string [@@deriving eq, ord, sexp]
end

module StringTripleSet = Set.Make(StringTriple)

module G = Imperative.Graph.Concrete(String)

module Clique = Clique.Bron_Kerbosch(G)

let read_lan_graph (filename: string) =
  let g = G.create () in
  In_channel.read_lines filename
  |> List.iter ~f:(fun line ->
    let [@warning "-8"] [c1; c2] = String.split line ~on:'-' in
    let v1 = G.V.create c1 in 
    G.add_vertex g v1;
    let v2 = G.V.create c2 in
    G.add_vertex g v2;
    G.add_edge g v1 v2;
  );
  g

let connections pairs = List.concat_map pairs ~f:(fun (c1, c2) -> [c1,c2; c2,c1])
|> Map.of_alist_multi (module String) 
|> Map.map ~f:(Set.of_list (module String))


let sorted (c1, c2, c3) = 
  let [@warning "-8"] [c1; c2; c3] = List.sort [c1; c2; c3] ~compare:String.compare in
  (c1, c2, c3)

let find_connected_triples (connections: string_map) (c1: string) = 
  let find c = Map.find connections c |> Option.value_exn in
  let c2s = find c1 in  
  let c2_connections c2 =
    let c3s = Set.inter c2s (find c2) |> Set.to_list in
    List.map c3s ~f:(fun c3 -> (c1, c2, c3)) in
  List.concat_map (Set.to_list c2s) ~f:c2_connections
  |> List.map ~f:sorted 
  |> StringTripleSet.of_list 
  |> Set.to_list

let has_computer_beginning_with_t (c1, c2, c3) =
  let begins_with_t s = Char.(String.get s 0 = 't') in
  begins_with_t c1 || begins_with_t c2 || begins_with_t c3 

let from_iter iter g =
  let vs = ref [] in
  iter (fun v -> vs := v :: !vs) g;
  !vs

let part_a filename =
  let graph = read_lan_graph filename in
  let computers = from_iter G.iter_vertex graph in
  let connections = from_iter G.iter_edges_e graph |> connections in
  let triples = List.concat_map computers ~f:(fun c1 ->
    find_connected_triples connections c1 |> List.filter ~f:has_computer_beginning_with_t
  ) |> StringTripleSet.of_list in
  Set.length triples

let part_b filename =
  read_lan_graph filename
  |> Clique.maximalcliques
  |> List.max_elt ~compare:(on List.length Int.compare)
  |> Option.value_exn
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","

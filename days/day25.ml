open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_graph =
  let open Adventofcode.Parser.Let_syntax in
  let entry =
    let%map vertex = to_char ':' <* whitespace
    and edges = String.split ~on:' ' <$> to_eol in
    vertex, edges
  in
  let%map vertices = some entry in
  let adjacencies = Hashtbl.of_alist_exn (module String) vertices in
  List.iter (Hashtbl.to_alist adjacencies) ~f:(fun (v, neighbours) ->
    List.iter neighbours ~f:(fun neighbour ->
      Hashtbl.add_multi adjacencies ~key:neighbour ~data:v));
  adjacencies
;;

let normalise_edge (a, b) = String.min a b, String.max a b

let find_non_removed_edges graph removed_edges v =
  Hashtbl.find graph v
  |> Option.value ~default:[]
  |> List.filter ~f:(fun neighbour ->
    not (Set.mem removed_edges (normalise_edge (v, neighbour))))
;;

let bfs graph removed_edges start target =
  let rec f queue seen =
    match queue with
    | [] -> None
    | (v, rev_path) :: _ when String.equal v target -> Some (List.rev rev_path)
    | (v, rev_path) :: rest ->
      let unseen_neighbours =
        find_non_removed_edges graph removed_edges v
        |> List.filter ~f:(Fn.non (Set.mem seen))
      in
      let seen' = List.fold unseen_neighbours ~init:seen ~f:Set.add in
      let queue' = rest @ List.map unseen_neighbours ~f:(fun n -> n, n :: rev_path) in
      f queue' seen'
  in
  let seen = Set.singleton (module String) start in
  f [ start, [ start ] ] seen
;;

module Edge = struct
  module T = struct
    type t = string * string [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

(* Repeatedly pick two random vertices and removes the edges in the shortest path between
   them until the graph becomes disconnected *)
let disconnect graph =
  let vertices = Hashtbl.keys graph |> List.to_array in
  let rec f removed_edges =
    let start = vertices.(Random.int (Array.length vertices)) in
    let target = vertices.(Random.int (Array.length vertices)) in
    match bfs graph removed_edges start target with
    | None -> removed_edges
    | Some path ->
      let edges =
        List.zip_with_remainder path (List.tl_exn path)
        |> fst
        |> List.map ~f:normalise_edge
      in
      let removed_edges' = List.fold edges ~init:removed_edges ~f:Set.add in
      f removed_edges'
  in
  f (Set.empty (module Edge))
;;

let rec component_size graph removed_edges seen v =
  match Hash_set.mem seen v with
  | true -> 0
  | _ ->
    Hash_set.add seen v;
    1
    + (find_non_removed_edges graph removed_edges v
       |> List.filter ~f:(Fn.non (Hash_set.mem seen))
       |> List.map ~f:(component_size graph removed_edges seen)
       |> List.reduce ~f:( + )
       |> Option.value ~default:0)
;;

let rec part_1 graph =
  let removed_edges = disconnect graph in
  let seen = Hash_set.create (module String) in
  let component_sizes =
    Hashtbl.keys graph
    |> List.map ~f:(component_size graph removed_edges seen)
    |> List.filter ~f:(( <> ) 0)
  in
  match component_sizes with
  | [ a; b ] -> a * b
  | _ ->
    (* Graph was disconnected into >2 connected components, try the whole process again *)
    part_1 graph
;;

let () =
  let input = read_input ~day:25 in
  let graph = run_exn parse_graph input in
  part_1 graph |> Int.to_string |> print_endline
;;

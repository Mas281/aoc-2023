open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, equal, hash, sexp]
  end

  include T
  include Comparator.Make (T)

  let in_bounds (row, col) grid =
    row >= 0 && row < Array.length grid && col >= 0 && col < Array.length grid.(0)
  ;;
end

let find_start grid =
  let col = Array.findi_exn grid.(0) ~f:(fun _ c -> Char.equal c '.') |> fst in
  0, col
;;

let can_enter_slopes grid (to_row, to_col) (from_row, from_col) =
  match grid.(to_row).(to_col) with
  | '.' -> true
  | 'v' -> Position.equal (to_row, to_col) (from_row + 1, from_col)
  | '^' -> Position.equal (to_row, to_col) (from_row - 1, from_col)
  | '>' -> Position.equal (to_row, to_col) (from_row, from_col + 1)
  | '<' -> Position.equal (to_row, to_col) (from_row, from_col - 1)
  | _ -> false
;;

let can_enter_flat grid (to_row, to_col) _ =
  match grid.(to_row).(to_col) with
  | '.' | 'v' | '^' | '>' | '<' -> true
  | _ -> false
;;

let accessible_cells grid ((row, col) as pos) prev ~can_enter =
  List.filter
    [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
    ~f:(fun pos' ->
      Position.in_bounds pos' grid
      && (not (Position.equal pos' prev))
      && can_enter grid pos' pos)
;;

module Node = struct
  type t = Position.t * int [@@deriving compare, equal]
end

let build_graph grid ~symmetric ~can_enter =
  let graph = Hashtbl.create (module Position) in
  let rec f path_start path_steps pos prev =
    let accessible = accessible_cells grid pos prev ~can_enter in
    match accessible with
    | [ next ] -> f path_start (path_steps + 1) next pos
    | _ ->
      (match symmetric with
       | false ->
         Hashtbl.add_multi graph ~key:path_start ~data:(pos, path_steps);
         List.iter accessible ~f:(fun pos' -> f pos 1 pos' pos)
       | _ ->
         let existing_paths = Hashtbl.find_multi graph path_start in
         if not (List.mem existing_paths (pos, path_steps) ~equal:Node.equal)
         then (
           Hashtbl.add_multi graph ~key:path_start ~data:(pos, path_steps);
           List.iter accessible ~f:(fun pos' -> f pos 1 pos' pos)))
  in
  let start = find_start grid in
  f start 0 start (-1, -1);
  (* We may have duplicate entries in the adjacency list if we reach the same junction
     via multiple paths *)
  Hashtbl.map_inplace graph ~f:(List.dedup_and_sort ~compare:Node.compare);
  graph
;;

let longest_path grid ~symmetric ~can_enter =
  let graph = build_graph grid ~symmetric ~can_enter in
  let rec max_distance ((row, _) as pos) visited =
    match row = Array.length grid - 1 with
    | true -> Some 0
    | _ ->
      let neighbours = Hashtbl.find_exn graph pos in
      List.filter neighbours ~f:(fun (pos', _) -> not (Set.mem visited pos'))
      |> List.filter_map ~f:(fun (pos', steps) ->
        let visited' = Set.add visited pos in
        let%map.Option to_end = max_distance pos' visited' in
        steps + to_end)
      |> List.max_elt ~compare:Int.compare
  in
  max_distance (find_start grid) (Set.empty (module Position)) |> Option.value_exn
;;

let part_1 = longest_path ~symmetric:false ~can_enter:can_enter_slopes
let part_2 = longest_path ~symmetric:true ~can_enter:can_enter_flat

let () =
  let input = read_input ~day:23 in
  let grid =
    let parse_grid = List.to_array <$> (List.map ~f:String.to_array <$> some to_eol) in
    run_exn parse_grid input
  in
  part_1 grid |> Int.to_string |> print_endline;
  part_2 grid |> Int.to_string |> print_endline
;;

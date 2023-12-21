open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Position = struct
  type t = int * int [@@deriving compare, hash, sexp_of]

  let in_bounds (row, col) grid =
    row >= 0 && row < Array.length grid && col >= 0 && col < Array.length grid.(0)
  ;;
end

let find_start grid =
  let row, _ = Array.findi_exn grid ~f:(fun _ a -> Array.mem a 'S' ~equal:Char.equal) in
  let col, _ = Array.findi_exn grid.(row) ~f:(fun _ c -> Char.equal c 'S') in
  grid.(row).(col) <- '.';
  row, col
;;

let adjacent_unseen grid seen (row, col) =
  List.filter
    [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
    ~f:(fun ((row', col') as pos) ->
      Position.in_bounds pos grid
      && Char.equal grid.(row').(col') '.'
      && not (Hash_set.mem seen pos))
;;

let count_reachable grid start_pos ~steps =
  let seen = Hash_set.create (module Position) in
  let rec bfs queue steps_moved (odd_reachable, even_reachable) =
    match steps_moved = steps || List.is_empty queue with
    | true -> odd_reachable, even_reachable
    | _ ->
      let queue' =
        List.map queue ~f:(adjacent_unseen grid seen)
        |> List.concat
        |> List.dedup_and_sort ~compare:Position.compare
      in
      List.iter queue' ~f:(Hash_set.add seen);
      let reachable' =
        match steps_moved % 2 with
        | 0 -> odd_reachable + List.length queue', even_reachable
        | _ -> odd_reachable, even_reachable + List.length queue'
      in
      bfs queue' (steps_moved + 1) reachable'
  in
  let odd_reachable, even_reachable = bfs [ start_pos ] 0 (0, 0) in
  match steps % 2 with
  | 1 -> odd_reachable
  | _ -> even_reachable
;;

let part_1 = count_reachable ~steps:64

let () =
  let input = read_input ~day:21 in
  let grid =
    let parse_grid = List.to_array <$> (List.map ~f:String.to_array <$> some to_eol) in
    run_exn parse_grid input
  in
  let start_pos = find_start grid in
  part_1 grid start_pos |> Int.to_string |> print_endline
;;

open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Direction = struct
  type t =
    | N
    | E
    | S
    | W
  [@@deriving compare, hash, sexp_of]

  let move t (row, col) =
    match t with
    | N -> row - 1, col
    | E -> row, col + 1
    | S -> row + 1, col
    | W -> row, col - 1
  ;;

  let next t = function
    | '/' ->
      (match t with
       | N -> [ E ]
       | E -> [ N ]
       | S -> [ W ]
       | W -> [ S ])
    | '\\' ->
      (match t with
       | N -> [ W ]
       | E -> [ S ]
       | S -> [ E ]
       | W -> [ N ])
    | '|' ->
      (match t with
       | E | W -> [ N; S ]
       | _ -> [ t ])
    | '-' ->
      (match t with
       | N | S -> [ E; W ]
       | _ -> [ t ])
    | _ -> [ t ]
  ;;
end

module Position = struct
  type t = int * int [@@deriving compare, hash, sexp_of]
end

module State = struct
  type t = Position.t * Direction.t [@@deriving compare, hash, sexp_of]
end

let in_bounds grid (row, col) =
  row >= 0 && row < Array.length grid && col >= 0 && col < String.length grid.(0)
;;

let count_energised grid (start_row, start_col) start_direction =
  let visited = Hash_set.create (module State) in
  let rec dfs ((row, col) as position) direction =
    match
      (not (in_bounds grid position)) || Hash_set.mem visited (position, direction)
    with
    | true -> ()
    | _ ->
      Hash_set.add visited (position, direction);
      let char = grid.(row).[col] in
      let new_directions = Direction.next direction char in
      List.iter new_directions ~f:(fun direction' ->
        let position' = Direction.move direction' position in
        dfs position' direction')
  in
  dfs (start_row, start_col) start_direction;
  let energised =
    Hash_set.to_list visited
    |> List.map ~f:fst
    |> List.dedup_and_sort ~compare:Position.compare
    |> List.length
  in
  energised
;;

let part_1 grid = count_energised grid (0, 0) Direction.E

let part_2 grid =
  let open Sequence.Let_syntax in
  let num_rows = Array.length grid in
  let num_cols = String.length grid.(0) in
  let rows = Sequence.range 0 num_rows in
  let cols = Sequence.range 0 num_cols in
  let top_starts = cols >>| fun col -> (0, col), Direction.S in
  let left_starts = rows >>| fun row -> (row, 0), Direction.E in
  let right_starts = rows >>| fun row -> (row, num_cols - 1), Direction.W in
  let bottom_starts = cols >>| fun col -> (num_rows - 1, col), Direction.S in
  let all_starts =
    Sequence.round_robin [ top_starts; left_starts; right_starts; bottom_starts ]
  in
  Sequence.map all_starts ~f:(Tuple2.uncurry (count_energised grid))
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let () =
  let input = read_input ~day:16 in
  let grid =
    let parse_grid = List.to_array <$> some to_eol in
    run_exn parse_grid input
  in
  part_1 grid |> Int.to_string |> print_endline;
  part_2 grid |> Int.to_string |> print_endline
;;

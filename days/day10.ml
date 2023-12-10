open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Position = struct
  type t = int * int [@@deriving compare, hash, sexp]
end

module Tile = struct
  type t =
    | North_south
    | East_west
    | North_east
    | North_west
    | South_west
    | South_east
    | Ground
    | Start
  [@@deriving equal]

  let parser = function
    | '|' :: rest -> Ok (rest, North_south)
    | '-' :: rest -> Ok (rest, East_west)
    | 'L' :: rest -> Ok (rest, North_east)
    | 'J' :: rest -> Ok (rest, North_west)
    | '7' :: rest -> Ok (rest, South_west)
    | 'F' :: rest -> Ok (rest, South_east)
    | '.' :: rest -> Ok (rest, Ground)
    | 'S' :: rest -> Ok (rest, Start)
    | _ -> Or_error.error_string "invalid tile"
  ;;
end

module Grid = struct
  type t = Tile.t array array

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    let%map lists = some (line (some Tile.parser)) in
    Array.of_list (List.map lists ~f:Array.of_list)
  ;;

  let find_start (t : t) =
    let row, row_tiles =
      Array.findi t ~f:(fun _ a -> Array.mem a Tile.Start ~equal:Tile.equal)
      |> Option.value_exn
    in
    let col, _ =
      Array.findi row_tiles ~f:(fun _ t -> Tile.equal t Tile.Start) |> Option.value_exn
    in
    row, col
  ;;

  let in_bounds t (row, col) =
    row >= 0 && row < Array.length t && col >= 0 && col < Array.length (Array.get t 0)
  ;;

  let next_position t (row, col) from_dir =
    let open Tile in
    let tile = t.(row).(col) in
    match from_dir with
    | `North ->
      (match tile with
       | North_south -> Some ((row + 1, col), `North)
       | North_east -> Some ((row, col + 1), `West)
       | North_west -> Some ((row, col - 1), `East)
       | _ -> None)
    | `East ->
      (match tile with
       | East_west -> Some ((row, col - 1), `East)
       | North_east -> Some ((row - 1, col), `South)
       | South_east -> Some ((row + 1, col), `North)
       | _ -> None)
    | `South ->
      (match tile with
       | North_south -> Some ((row - 1, col), `South)
       | South_east -> Some ((row, col + 1), `West)
       | South_west -> Some ((row, col - 1), `East)
       | _ -> None)
    | `West ->
      (match tile with
       | East_west -> Some ((row, col + 1), `West)
       | North_west -> Some ((row - 1, col), `South)
       | South_west -> Some ((row + 1, col), `North)
       | _ -> None)
  ;;
end

let get_loop grid start from_dir =
  let rec f path (row, col) from_dir =
    match Grid.in_bounds grid (row, col) with
    | false -> None
    | true ->
      (match grid.(row).(col) with
       | Tile.Start -> Some path
       | _ ->
         let%bind.Option next_position, next_dir =
           Grid.next_position grid (row, col) from_dir
         in
         f (next_position :: path) next_position next_dir)
  in
  f [ start ] start from_dir
;;

let part_1 grid =
  let start_row, start_col = Grid.find_start grid in
  let potential_loops =
    [ get_loop grid (start_row - 1, start_col) `South
    ; get_loop grid (start_row, start_col + 1) `West
    ; get_loop grid (start_row + 1, start_col) `North
    ; get_loop grid (start_row, start_col - 1) `East
    ]
  in
  let loop = List.filter_opt potential_loops |> List.hd_exn in
  loop
;;

(* If a non-loop cell is preceded by an odd number of "vertical" loop cells, then it is
   enclosed by the loop *)
let enclosed_on_row grid loop row =
  let cols = Sequence.range 0 (Array.length grid.(0)) in
  Sequence.fold cols ~init:(false, 0) ~f:(fun (inside, num_inside) col ->
    let part_of_loop = Hash_set.mem loop (row, col) in
    let inside' =
      if not part_of_loop
      then inside
      else (
        (* Flip [inside] if we encounter a vertical loop part *)
        match grid.(row).(col) with
        | Tile.North_south | North_east | North_west | Start -> not inside
        | _ -> inside)
    in
    let num_inside' =
      match inside' && not part_of_loop with
      | true -> num_inside + 1
      | _ -> num_inside
    in
    inside', num_inside')
  |> snd
;;

let part_2 grid loop =
  let loop = Hash_set.of_list (module Position) loop in
  let rows = Sequence.range 0 (Array.length grid) in
  Sequence.map rows ~f:(enclosed_on_row grid loop) |> Sequence.reduce_exn ~f:( + )
;;

let () =
  let input = read_input ~day:10 in
  let grid = run_exn Grid.parse input in
  let loop = part_1 grid in
  let max_distance = List.length loop / 2 in
  max_distance |> Int.to_string |> print_endline;
  part_2 grid loop |> Int.to_string |> print_endline
;;

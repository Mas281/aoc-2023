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

let unseen_neighbours grid seen (row, col) =
  List.filter
    [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
    ~f:(fun ((row', col') as pos) ->
      Position.in_bounds pos grid
      && Char.equal grid.(row').(col') '.'
      && not (Hash_set.mem seen pos))
;;

let unseen_neighbours_infinite grid seen (row, col) =
  List.filter
    [ row + 1, col; row - 1, col; row, col + 1; row, col - 1 ]
    ~f:(fun ((row', col') as pos) ->
      Char.equal grid.(row' % Array.length grid).(col' % Array.length grid.(0)) '.'
      && not (Hash_set.mem seen pos))
;;

let count_reachable grid start_pos ~steps ~neighbours =
  let seen = Hash_set.create (module Position) in
  let rec bfs queue steps_moved (odd_reachable, even_reachable) =
    match steps_moved = steps || List.is_empty queue with
    | true -> odd_reachable, even_reachable
    | _ ->
      let queue' =
        List.map queue ~f:(neighbours grid seen)
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

let part_1 = count_reachable ~steps:64 ~neighbours:unseen_neighbours

(* My input grid has size 131x131. We want to find the number of reachable points after
   exactly 26501365 steps. Conveniently, 26501365 = 202300 * 131 + 65, and due to some
   properties of the input a sequence forms:
   Let f(i) = reachable(65 + i * 131)
   It turns out that f is a quadratic. By finding the first three values we can solve for
   the coefficients, and then f(202300) is our answer. The code below performs this logic
   for a general input that follows the same assumptions *)
let part_2 grid start_pos =
  let steps = 26501365 in
  let length = Array.length grid in
  let remainder = steps % length in
  let f i =
    count_reachable
      grid
      start_pos
      ~steps:(remainder + (i * length))
      ~neighbours:unseen_neighbours_infinite
  in
  let f0, f1, f2 = f 0, f 1, f 2 in
  (* The values of a, b, c follow from solving simultaneous equations:
     a * 0^2 + b * 0 + c = f0
     a * 1^2 + b * 1 + c = f1
     a * 2^2 + b * 2 + c = f2
  *)
  let c = f0 in
  let a = ((f0 + f2) / 2) - f1 in
  let b = f1 - f0 - a in
  let x = steps / length in
  (a * x * x) + (b * x) + c
;;

let () =
  let input = read_input ~day:21 in
  let grid =
    let parse_grid = List.to_array <$> (List.map ~f:String.to_array <$> some to_eol) in
    run_exn parse_grid input
  in
  let start_pos = find_start grid in
  part_1 grid start_pos |> Int.to_string |> print_endline;
  part_2 grid start_pos |> Int.to_string |> print_endline
;;

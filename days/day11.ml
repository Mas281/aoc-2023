open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let find_galaxies grid =
  List.foldi grid ~init:[] ~f:(fun row acc l ->
    let row_galaxies =
      List.filter_mapi l ~f:(fun col ->
          function
          | '#' -> Some (row, col)
          | _ -> None)
    in
    row_galaxies :: acc)
  |> List.concat
;;

let find_expansions grid =
  let expansions rows =
    List.filter_mapi rows ~f:(fun row l ->
      match List.for_all l ~f:(Char.equal '.') with
      | true -> Some row
      | false -> None)
  in
  let row_expansions = expansions grid |> Hash_set.of_list (module Int) in
  let col_expansions =
    expansions (List.transpose_exn grid) |> Hash_set.of_list (module Int)
  in
  row_expansions, col_expansions
;;

let distance (row1, col1) (row2, col2) (row_expansions, col_expansions) ~expansion_factor =
  let axis_distance p1 p2 expansions =
    let p1, p2 = Int.min p1 p2, Int.max p1 p2 in
    let range = Sequence.range p1 p2 in
    let expanded_count = Sequence.count range ~f:(Hash_set.mem expansions) in
    let not_expanded_count = p2 - p1 - expanded_count in
    (expanded_count * expansion_factor) + not_expanded_count
  in
  let row_distance = axis_distance row1 row2 row_expansions in
  let col_distance = axis_distance col1 col2 col_expansions in
  row_distance + col_distance
;;

let sum_of_distances galaxies expansions ~expansion_factor =
  let rec f = function
    | [] -> 0
    | p1 :: tl ->
      let distances_from_p1 =
        List.fold tl ~init:0 ~f:(fun acc p2 ->
          acc + distance p1 p2 expansions ~expansion_factor)
      in
      distances_from_p1 + f tl
  in
  f galaxies
;;

let part_1 = sum_of_distances ~expansion_factor:2
let part_2 = sum_of_distances ~expansion_factor:1000000

let () =
  let input = read_input ~day:11 in
  let grid =
    let parse_grid = List.map ~f:String.to_list <$> some to_eol in
    run_exn parse_grid input
  in
  let galaxies = find_galaxies grid in
  let expansions = find_expansions grid in
  part_1 galaxies expansions |> Int.to_string |> print_endline;
  part_2 galaxies expansions |> Int.to_string |> print_endline
;;

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
    |> List.sort ~compare:Int.compare
    |> List.to_array
  in
  let row_expansions = expansions grid in
  let col_expansions = expansions (List.transpose_exn grid) in
  row_expansions, col_expansions
;;

let count_expansions_in_interval expansions low high =
  let open Option.Let_syntax in
  (let%bind first_expansion =
     Array.binary_search expansions `First_strictly_greater_than low ~compare:Int.compare
   in
   let%map last_expansion =
     Array.binary_search expansions `Last_strictly_less_than high ~compare:Int.compare
   in
   last_expansion - first_expansion + 1)
  |> Option.value ~default:0
;;

let distance (row1, col1) (row2, col2) (row_expansions, col_expansions) ~expansion_factor =
  let axis_distance p1 p2 expansions =
    let p1, p2 = Int.min p1 p2, Int.max p1 p2 in
    let num_expanded = count_expansions_in_interval expansions p1 p2 in
    let num_non_expanded = p2 - p1 - num_expanded in
    (expansion_factor * num_expanded) + num_non_expanded
  in
  let row_distance = axis_distance row1 row2 row_expansions in
  let col_distance = axis_distance col1 col2 col_expansions in
  row_distance + col_distance
;;

let sum_of_distances galaxies expansions ~expansion_factor =
  let rec f = function
    | [] -> 0
    | galaxy :: tl ->
      let total_distance_from_galaxy =
        List.fold tl ~init:0 ~f:(fun acc other ->
          acc + distance galaxy other expansions ~expansion_factor)
      in
      total_distance_from_galaxy + f tl
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

open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let differences l =
  List.map l ~f:(fun (l1, l2) ->
    List.zip_exn l1 l2
    |> List.map ~f:(fun (a, b) -> not (Char.equal a b))
    |> List.count ~f:Fn.id)
  |> List.reduce_exn ~f:( + )
;;

let rows_before_symmetry grid ~smudges =
  let rows = Sequence.range 1 (List.length grid) in
  Sequence.find rows ~f:(fun row ->
    let upper_rows, lower_rows = List.split_n grid row in
    let upper_rows_rev = List.rev upper_rows in
    let zipped = List.zip_with_remainder upper_rows_rev lower_rows |> fst in
    differences zipped = smudges)
;;

let columns_before_symmetry = Fn.compose rows_before_symmetry List.transpose_exn

let summarise_grids grids ~smudges =
  List.map grids ~f:(fun grid ->
    match rows_before_symmetry grid ~smudges with
    | Some n -> 100 * n
    | None -> columns_before_symmetry grid ~smudges |> Option.value_exn)
  |> List.reduce_exn ~f:( + )
;;

let part_1 = summarise_grids ~smudges:0
let part_2 = summarise_grids ~smudges:1

let () =
  let input = read_input ~day:13 in
  let parse_grids =
    let grid = some (String.to_list <$> to_eol) in
    list grid ~sep:consume_line
  in
  let grids = run_exn parse_grids input in
  part_1 grids |> Int.to_string |> print_endline;
  part_2 grids |> Int.to_string |> print_endline
;;

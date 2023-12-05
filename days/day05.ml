open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_maps =
  let open Adventofcode.Parser.Let_syntax in
  let range =
    let%map dest_start = int <* whitespace
    and src_start = int <* whitespace
    and length = int <* whitespace in
    src_start, dest_start, length
  in
  let map = consume upto_newline *> some range in
  let%map seeds = consume (string "seeds: ") *> some (int <* whitespace)
  and maps = some map in
  seeds, maps
;;

let value_from_range source map =
  List.find_map map ~f:(fun (src_start, dest_start, length) ->
    match source >= src_start && source < src_start + length with
    | true -> Some (dest_start + source - src_start)
    | false -> None)
  |> Option.value ~default:source
;;

let location seed = List.fold ~init:seed ~f:value_from_range

let part_1 seeds maps =
  List.map seeds ~f:(fun seed -> location seed maps)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let min_location (start, length) maps =
  let rec f seed min =
    match seed < start + length with
    | true ->
      let new_min = Int.min min (location seed maps) in
      f (seed + 1) new_min
    | false -> min
  in
  f start Int.max_value
;;

let part_2 seeds maps =
  let pairs =
    List.chunks_of seeds ~length:2
    |> List.map ~f:(function
      | [ a; b ] -> a, b
      | _ -> failwith "odd length")
  in
  List.map pairs ~f:(fun range -> min_location range maps)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let () =
  let input = read_input ~day:5 in
  let seeds, maps = run_exn parse_maps input in
  part_1 seeds maps |> Int.to_string |> print_endline;
  part_2 seeds maps |> Int.to_string |> print_endline
;;

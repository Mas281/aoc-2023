open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_maps =
  let open Adventofcode.Parser.Let_syntax in
  let range =
    let%map dest = int <* whitespace
    and src = int <* whitespace
    and length = int <* whitespace in
    src, dest, length
  in
  let map = consume upto_newline *> some range in
  let%map seeds = consume (string "seeds: ") *> some (int <* whitespace)
  and maps = some map in
  seeds, maps
;;

let range_map input map =
  List.find_map map ~f:(fun (src, dest, length) ->
    match input >= src && input < src + length with
    | true -> Some (dest + input - src)
    | false -> None)
  |> Option.value ~default:input
;;

let apply_range_maps ~init = List.fold ~init ~f:range_map

let part_1 seeds maps =
  List.map seeds ~f:(fun seed -> apply_range_maps ~init:seed maps)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
;;

let part_2 seeds maps =
  let seed_ranges =
    List.chunks_of seeds ~length:2
    |> List.map ~f:(function
      | [ a; b ] -> a, b
      | _ -> failwith "odd length")
  in
  let inverse_maps =
    List.map maps ~f:(List.map ~f:(fun (src, dest, length) -> dest, src, length))
    |> List.rev
  in
  let rec find_lowest_seed location =
    let seed = apply_range_maps ~init:location inverse_maps in
    match
      List.exists seed_ranges ~f:(fun (start, length) ->
        seed >= start && seed < start + length)
    with
    | true -> location
    | false -> find_lowest_seed (location + 1)
  in
  find_lowest_seed 0
;;

let () =
  let input = read_input ~day:5 in
  let seeds, maps = run_exn parse_maps input in
  part_1 seeds maps |> Int.to_string |> print_endline;
  part_2 seeds maps |> Int.to_string |> print_endline
;;

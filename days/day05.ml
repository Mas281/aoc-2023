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
  let map = consume to_eol *> some range in
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

let apply_map_rule_to_range (src, dest, length) (range_start, range_end) =
  let rule_start, rule_end = src, src + length in
  let unmapped_before =
    match range_start < rule_start with
    | true -> Some (range_start, Int.min range_end rule_start)
    | false -> None
  in
  let unmapped_after =
    match range_end > rule_end with
    | true -> Some (Int.max range_start rule_end, range_end)
    | false -> None
  in
  let intersection =
    match
      (range_start >= rule_start && range_start <= rule_end)
      || (range_end >= rule_start && range_end <= rule_end)
    with
    | true ->
      let offset = dest - src in
      Some (offset + Int.max range_start rule_start, offset + Int.min range_end rule_end)
    | false -> None
  in
  let unmapped = [ unmapped_before; unmapped_after ] in
  let mapped = [ intersection ] in
  List.filter_opt unmapped, List.filter_opt mapped
;;

let apply_map_to_ranges ranges map =
  let rec f unmapped mapped = function
    | [] -> unmapped @ mapped
    | rule :: rest ->
      let new_unmapped, new_mapped =
        List.map unmapped ~f:(apply_map_rule_to_range rule)
        |> List.unzip
        |> Tuple2.map ~f:List.concat
      in
      f new_unmapped (new_mapped @ mapped) rest
  in
  f ranges [] map
;;

let part_2 seeds maps =
  let seed_ranges =
    List.chunks_of seeds ~length:2
    |> List.map ~f:(function
      | [ start; length ] -> start, start + length
      | _ -> failwith "odd length")
  in
  let mapped_ranges = List.fold maps ~init:seed_ranges ~f:apply_map_to_ranges in
  let min_location =
    let compare = Comparable.lift Int.compare ~f:fst in
    List.min_elt mapped_ranges ~compare |> Option.value_exn |> fst
  in
  min_location
;;

let () =
  let input = read_input ~day:5 in
  let seeds, maps = run_exn parse_maps input in
  part_1 seeds maps |> Int.to_string |> print_endline;
  part_2 seeds maps |> Int.to_string |> print_endline
;;

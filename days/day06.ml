open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_races =
  let open Adventofcode.Parser.Let_syntax in
  let%map times = consume (string "Time:") *> whitespace *> some (int <* whitespace)
  and distances =
    consume (string "Distance:") *> whitespace *> some (int <* whitespace)
  in
  times, distances
;;

let ways_to_win (time, distance) =
  let open Float.O in
  let time, distance = Float.of_int time, Float.of_int distance in
  (* Number of integers satisfying x^2 - time * x + distance < 0 *)
  let sqrtDiscriminant = (time * time) - (4.0 * distance) |> Float.sqrt in
  let alpha = (time - sqrtDiscriminant) / 2.0 in
  let beta = (time + sqrtDiscriminant) / 2.0 in
  Float.round_down beta - Float.round_down alpha |> Int.of_float
;;

let part_1 times distances =
  let races = List.zip_exn times distances in
  List.map races ~f:ways_to_win |> List.reduce_exn ~f:( * )
;;

let part_2 times distances =
  let time = List.map times ~f:Int.to_string |> String.concat |> Int.of_string in
  let distance = List.map distances ~f:Int.to_string |> String.concat |> Int.of_string in
  ways_to_win (time, distance)
;;

let () =
  let input = read_input ~day:6 in
  let times, distances = run_exn parse_races input in
  part_1 times distances |> Int.to_string |> print_endline;
  part_2 times distances |> Int.to_string |> print_endline
;;

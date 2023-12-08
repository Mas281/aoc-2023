open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_map =
  let open Adventofcode.Parser.Let_syntax in
  let node =
    let%map id = to_whitespace <* consume (string "= (")
    and left = to_char ',' <* whitespace
    and right = to_char ')' <* whitespace in
    id, (left, right)
  in
  let%map instructions = to_whitespace
  and nodes = some node in
  instructions, Hashtbl.of_alist_exn (module String) nodes
;;

let rec num_steps instructions map steps location =
  match String.is_suffix location ~suffix:"Z" with
  | true -> steps
  | false ->
    let left, right = Hashtbl.find_exn map location in
    let next =
      match instructions.[steps % String.length instructions] with
      | 'L' -> left
      | _ -> right
    in
    num_steps instructions map (steps + 1) next
;;

let part_1 instructions map = num_steps instructions map 0 "AAA"

let part_2 instructions map =
  let starts = List.filter (Hashtbl.keys map) ~f:(String.is_suffix ~suffix:"A") in
  let steps_each = List.map starts ~f:(num_steps instructions map 0) in
  let rec gcd a b =
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)
  in
  let lcm a b = a * b / gcd a b in
  List.reduce_exn steps_each ~f:lcm
;;

let () =
  let input = read_input ~day:8 in
  let instructions, map = run_exn parse_map input in
  part_1 instructions map |> Int.to_string |> print_endline;
  part_2 instructions map |> Int.to_string |> print_endline
;;

open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_sequences = some (list int ~sep:(char ' ') <* consume_line)

let rec next_value sequence =
  match List.for_all sequence ~f:(( = ) 0) with
  | true -> 0
  | false ->
    let last = List.last_exn sequence in
    let differences =
      let except_last = List.take sequence (List.length sequence - 1) in
      List.map2_exn ~f:( - ) (List.tl_exn sequence) except_last
    in
    last + next_value differences
;;

let part_1 sequences = List.map sequences ~f:next_value |> List.reduce_exn ~f:( + )
let part_2 sequences = List.map sequences ~f:List.rev |> part_1

let () =
  let input = read_input ~day:9 in
  let sequences = run_exn parse_sequences input in
  part_1 sequences |> Int.to_string |> print_endline;
  part_2 sequences |> Int.to_string |> print_endline
;;

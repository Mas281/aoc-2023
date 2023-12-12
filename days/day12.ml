open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_rows =
  let open Adventofcode.Parser.Let_syntax in
  let row =
    let%map springs = to_whitespace
    and groups = list int ~sep:(char ',') <* whitespace in
    springs, Array.of_list groups
  in
  some row
;;

module Key = struct
  module T = struct
    type t = int * int * int [@@deriving compare, hash, sexp_of]
  end

  include Hashable.Make_plain (T)
end

let count_arrangements (springs, groups) =
  (*
     Dynamic programming:
     - spring_idx: the index into [springs] of the current spring being looked at
     - group_idx: the index into [groups] of the current group of damaged springs
     - consecutive: the number of consecutive damaged springs in the current group
  *)
  let f recurse (spring_idx, group_idx, consecutive) =
    (* If we have exhausted all the groups of damaged springs, we have a valid solution iff
       there are no '#'s in the remainder of the string *)
    if group_idx >= Array.length groups
    then (
      match
        String.for_all (String.subo springs ~pos:spring_idx) ~f:(Fn.non (Char.equal '#'))
      with
      | true -> 1
      | false -> 0)
    else if spring_idx >= String.length springs
    then (
      (* Otherwise if we have exhausted all the springs, we must have the correct number of
         consecutive damaged springs to satisfy the final group *)
      match group_idx = Array.length groups - 1 && consecutive = Array.last groups with
      | true -> 1
      | false -> 0)
    else (
      (* Otherwise recursively try possibilities based on the observed character *)
      match springs.[spring_idx] with
      | '#' -> recurse (spring_idx + 1, group_idx, consecutive + 1)
      | '.' ->
        (* If we see a '.' and there is an ongoing group, check that the group was of the
           correct size *)
        if consecutive = 0
        then recurse (spring_idx + 1, group_idx, 0)
        else (
          match consecutive = groups.(group_idx) with
          | true -> recurse (spring_idx + 1, group_idx + 1, 0)
          | false -> 0)
      | _ ->
        (* If we see a '?' and there is no ongoing group, we can either choose to start
           a new group at this index or do nothing *)
        if consecutive = 0
        then
          recurse (spring_idx + 1, group_idx, 0) + recurse (spring_idx + 1, group_idx, 1)
        else (
          (* Otherwise if there is an ongoing group:
             - Continue it if it isn't long enough
             - End it if it's the right size
             - Fail if it's too long *)
          match Int.compare consecutive groups.(group_idx) with
          | x when x < 0 -> recurse (spring_idx + 1, group_idx, consecutive + 1)
          | 0 -> recurse (spring_idx + 1, group_idx + 1, 0)
          | _ -> 0))
  in
  let memoised = Memo.recursive f ~hashable:Key.hashable in
  memoised (0, 0, 0)
;;

let part_1 rows = List.map rows ~f:count_arrangements |> List.reduce_exn ~f:( + )

let part_2 rows =
  let repeat_springs springs =
    String.concat [ springs; "?"; springs; "?"; springs; "?"; springs; "?"; springs ]
  in
  let repeat_groups groups = Array.concat [ groups; groups; groups; groups; groups ] in
  List.map rows ~f:(fun (springs, groups) -> repeat_springs springs, repeat_groups groups)
  |> part_1
;;

let () =
  let input = read_input ~day:12 in
  let rows = run_exn parse_rows input in
  part_1 rows |> Int.to_string |> print_endline;
  part_2 rows |> Int.to_string |> print_endline
;;

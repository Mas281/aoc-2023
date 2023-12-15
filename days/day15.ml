open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let hash = String.fold ~init:0 ~f:(fun acc c -> 17 * (acc + Char.to_int c) % 256)
let part_1 raw_steps = List.map raw_steps ~f:hash |> List.reduce_exn ~f:( + )

module Step = struct
  type t =
    | Add of string * int
    | Remove of string

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    let add =
      let%map label = predicate ~f:Char.is_alpha
      and () = consume_char '='
      and focal_length = int in
      Add (label, focal_length)
    in
    let remove =
      let%map label = predicate ~f:Char.is_alpha
      and () = consume_char '-' in
      Remove label
    in
    add <|> remove
  ;;
end

let add_label (to_add, focal_length) box =
  let rec f = function
    | [] -> [ to_add, focal_length ]
    | (label, _) :: rest when String.equal label to_add -> (to_add, focal_length) :: rest
    | hd :: rest -> hd :: f rest
  in
  f box
;;

let remove_label to_remove =
  List.filter ~f:(fun (label, _) -> not (String.equal label to_remove))
;;

let part_2 steps =
  let boxes = Array.init 256 ~f:(Fn.const []) in
  List.iter steps ~f:(function
    | Step.Add (label, focal_length) ->
      let box = hash label in
      boxes.(box) <- add_label (label, focal_length) boxes.(box)
    | Remove label ->
      let box = hash label in
      boxes.(box) <- remove_label label boxes.(box));
  let focusing_power box =
    List.mapi box ~f:(fun i (_, focal_length) -> (i + 1) * focal_length)
    |> List.reduce ~f:( + )
    |> Option.value ~default:0
  in
  Array.mapi boxes ~f:(fun i box -> (i + 1) * focusing_power box)
  |> Array.reduce_exn ~f:( + )
;;

let () =
  let input = read_input ~day:15 in
  let raw_steps = String.rstrip input |> String.split ~on:',' in
  part_1 raw_steps |> Int.to_string |> print_endline;
  let steps = List.map raw_steps ~f:(run_exn Step.parse) in
  part_2 steps |> Int.to_string |> print_endline
;;

open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_games =
  let open Adventofcode.Parser.Let_syntax in
  let set =
    Hashtbl.of_alist_exn (module String)
    <$> list
          (let%map num = int <* whitespace
           and colour = predicate ~f:Char.is_alpha in
           colour, num)
          ~sep:(string ", ")
  in
  let game =
    let%map game_no = consume (string "Game ") *> int <* consume (string ": ")
    and sets = list set ~sep:(string "; ") in
    game_no, sets
  in
  some (line game)
;;

let find_or_default table key =
  match Hashtbl.find table key with
  | Some x -> x
  | None -> 0
;;

let possible (_, sets) ~red ~blue ~green =
  List.for_all sets ~f:(fun set ->
    find_or_default set "red" <= red
    && find_or_default set "blue" <= blue
    && find_or_default set "green" <= green)
;;

let part_1 games =
  List.filter games ~f:(possible ~red:12 ~green:13 ~blue:14)
  |> List.map ~f:fst
  |> List.reduce_exn ~f:( + )
;;

let power (_, sets) =
  let min_r, min_g, min_b =
    List.fold sets ~init:(1, 1, 1) ~f:(fun (r, g, b) set ->
      ( Int.max r (find_or_default set "red")
      , Int.max g (find_or_default set "green")
      , Int.max b (find_or_default set "blue") ))
  in
  min_r * min_g * min_b
;;

let part_2 games = List.map games ~f:power |> List.reduce_exn ~f:( + )

let () =
  let input = read_input ~day:2 in
  let games = run_exn parse_games input in
  part_1 games |> Int.to_string |> print_endline;
  part_2 games |> Int.to_string |> print_endline
;;

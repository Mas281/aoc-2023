open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_cards =
  let open Adventofcode.Parser.Let_syntax in
  let card =
    let%map card_no =
      consume (string "Card" <* whitespace) *> int <* consume (string ":") <* whitespace
    and winning = some (int <* whitespace)
    and () = consume_char '|' <* whitespace
    and ours = some (int <* whitespace) in
    let matching = List.count ours ~f:(List.mem winning ~equal:Int.equal) in
    card_no, matching
  in
  some card
;;

let part_1 cards =
  List.map cards ~f:(fun (_, matching) ->
    match matching with
    | 0 -> 0
    | n -> Int.pow 2 (n - 1))
  |> List.reduce_exn ~f:( + )
;;

let part_2 cards =
  let table = Hashtbl.of_alist_exn (module Int) cards in
  let rec cards_won card_no =
    let matching = Hashtbl.find_exn table card_no in
    let recursively_won =
      List.range (card_no + 1) (card_no + matching) ~stop:`inclusive
      |> List.map ~f:cards_won
      |> List.reduce ~f:( + )
      |> Option.value ~default:0
    in
    matching + recursively_won
  in
  let original_cards = List.length cards in
  let new_cards =
    List.map cards ~f:(Fn.compose cards_won fst) |> List.reduce_exn ~f:( + )
  in
  original_cards + new_cards
;;

let () =
  let input = read_input ~day:4 in
  let cards = run_exn parse_cards input in
  part_1 cards |> Int.to_string |> print_endline;
  part_2 cards |> Int.to_string |> print_endline
;;

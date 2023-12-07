open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_hands =
  let open Adventofcode.Parser.Let_syntax in
  let hand =
    let%map cards = until_whitespace
    and bid = int <* whitespace in
    cards, bid
  in
  some hand
;;

let symbols = String.to_list "AKQJT98765432"

let normal_hand_type cards =
  let counts =
    List.map symbols ~f:(fun s -> String.count cards ~f:(Char.equal s))
    |> List.sort ~compare:Int.descending
  in
  match List.hd_exn counts, List.nth_exn counts 1 with
  | 5, _ -> 10 (* Five of a kind *)
  | 4, _ -> 9 (* Four of a kind *)
  | 3, 2 -> 8 (* Full house *)
  | 3, _ -> 7 (* Three of a kind *)
  | 2, 2 -> 6 (* Two pair *)
  | 2, _ -> 5 (* One pair *)
  | 1, _ -> 4 (* High card *)
  | _, _ -> failwith "invalid hand"
;;

let joker_hand_type cards =
  List.map symbols ~f:(fun s ->
    String.substr_replace_all cards ~pattern:"J" ~with_:(String.of_char s))
  |> List.map ~f:normal_hand_type
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let compare_hands (type1, cards1, _) (type2, cards2, _) ~symbol_order =
  match type1 <> type2 with
  | true -> Int.compare type1 type2
  | false ->
    let position_in_symbol_order c =
      List.findi_exn symbol_order ~f:(fun _ s -> Char.equal s c) |> fst
    in
    let compare = Comparable.lift Int.descending ~f:position_in_symbol_order in
    List.compare compare (String.to_list cards1) (String.to_list cards2)
;;

let winnings hands ~hand_type ~symbol_order =
  List.map hands ~f:(fun (cards, bid) -> hand_type cards, cards, bid)
  |> List.sort ~compare:(compare_hands ~symbol_order)
  |> List.mapi ~f:(fun i (_, _, bid) -> (i + 1) * bid)
  |> List.reduce_exn ~f:( + )
;;

let part_1 = winnings ~hand_type:normal_hand_type ~symbol_order:symbols

let part_2 =
  let joker_symbol_order = String.to_list "AKQT98765432J" in
  winnings ~hand_type:joker_hand_type ~symbol_order:joker_symbol_order
;;

let () =
  let input = read_input ~day:7 in
  let hands = run_exn parse_hands input in
  part_1 hands |> Int.to_string |> print_endline;
  part_2 hands |> Int.to_string |> print_endline
;;

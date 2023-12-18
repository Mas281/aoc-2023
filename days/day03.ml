open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let part_1 input line_length =
  let symbol_adjacent i =
    List.exists
      [ i - 1
      ; i + 1
      ; i - line_length
      ; i + line_length
      ; i - line_length - 1
      ; i + line_length - 1
      ; i - line_length + 1
      ; i + line_length + 1
      ]
      ~f:(fun idx ->
        idx >= 0
        && idx < String.length input
        &&
        match input.[idx] with
        | '.' | '\n' -> false
        | c when Char.is_digit c -> false
        | _ -> true)
  in
  String.foldi input ~init:(0, false, 0) ~f:(fun idx (acc, adjacent, sum) ->
    function
    | c when Char.is_digit c ->
      let digit = Char.get_digit_exn c in
      (10 * acc) + digit, adjacent || symbol_adjacent idx, sum
    | _ -> 0, false, if adjacent then sum + acc else sum)
  |> trd3
;;

let part_2 input line_length =
  let is_digit idx = idx >= 0 && idx < String.length input && Char.is_digit input.[idx] in
  let rec expand idx ~delta =
    match is_digit idx with
    | false -> ""
    | true -> Char.to_string input.[idx] ^ expand (idx + delta) ~delta
  in
  let expand_left idx = expand idx ~delta:(-1) |> String.rev in
  let expand_right = expand ~delta:1 in
  let expand_around idx =
    let left = expand_left (idx - 1) in
    let right = expand_right (idx + 1) in
    let upper_left = expand_left (idx - line_length - 1) in
    let upper_right = expand_right (idx - line_length + 1) in
    let lower_left = expand_left (idx + line_length - 1) in
    let lower_right = expand_right (idx + line_length + 1) in
    let above =
      (* If there's a digit directly above the gear, merge everything above into one long
         number, else take the diagonals separately *)
      match is_digit (idx - line_length) with
      | true -> [ upper_left ^ Char.to_string input.[idx - line_length] ^ upper_right ]
      | false -> [ upper_left; upper_right ]
    in
    let below =
      (* Same logic as above *)
      match is_digit (idx + line_length) with
      | true -> [ lower_left ^ Char.to_string input.[idx + line_length] ^ lower_right ]
      | false -> [ lower_left; lower_right ]
    in
    [ left; right ] @ above @ below
  in
  String.foldi input ~init:0 ~f:(fun idx total ->
    function
    | '*' ->
      let part_numbers =
        expand_around idx
        |> List.filter ~f:(Fn.non String.is_empty)
        |> List.map ~f:Int.of_string
      in
      (match part_numbers with
       | [ x; y ] -> total + (x * y)
       | _ -> total)
    | _ -> total)
;;

let () =
  let input = read_input ~day:3 in
  let line_length = 1 + String.index_exn input '\n' in
  part_1 input line_length |> Int.to_string |> print_endline;
  part_2 input line_length |> Int.to_string |> print_endline
;;

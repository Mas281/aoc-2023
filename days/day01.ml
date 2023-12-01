open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let part_1 strings =
  List.map ~f:(String.filter ~f:Char.is_digit) strings
  |> List.map ~f:(fun s -> String.make 1 s.[0] ^ String.make 1 s.[String.length s - 1])
  |> List.map ~f:Int.of_string
  |> List.reduce_exn ~f:( + )
;;

let digits =
  [ "one", 1
  ; "two", 2
  ; "three", 3
  ; "four", 4
  ; "five", 5
  ; "six", 6
  ; "seven", 7
  ; "eight", 8
  ; "nine", 9
  ]
  |> List.map ~f:(fun (s, x) -> String.to_list s, x)
;;

let digits_in_string s =
  let rec f chars =
    match chars with
    | [] -> []
    | c :: rest when Char.is_digit c ->
      let digit = String.make 1 c |> Int.of_string in
      digit :: f rest
    | _ :: rest ->
      (match
         List.find digits ~f:(fun (word, _) ->
           List.is_prefix chars ~prefix:word ~equal:Char.equal)
       with
       | Some (_, digit) -> digit :: f rest
       | None -> f rest)
  in
  f (String.to_list s)
;;

let part_2 strings =
  List.map strings ~f:(fun s ->
    let digits = digits_in_string s in
    (10 * List.hd_exn digits) + List.last_exn digits)
  |> List.reduce_exn ~f:( + )
;;

let () =
  let input = read_input ~day:1 in
  let strings = run_exn (some until_whitespace) input in
  part_1 strings |> Int.to_string |> print_endline;
  part_2 strings |> Int.to_string |> print_endline
;;

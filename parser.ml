open! Core

module Parser_applicative = struct
  module T = struct
    type 'a t = char list -> (char list * 'a) Or_error.t

    let return x input = Ok (input, x)

    let apply tf ta input =
      let open Or_error.Let_syntax in
      let%bind rest, f = tf input in
      let%map rest', a = ta rest in
      rest', f a
    ;;

    let map = `Define_using_apply
  end

  include T
  include Applicative.Make (T)
end

include Parser_applicative

module Open_on_rhs_intf = struct
  module type S = sig end
end

include
  Applicative.Make_let_syntax (Parser_applicative) (Open_on_rhs_intf) (Parser_applicative)

let ( <$> ) f t = return f <*> t

let ( <|> ) ta tb input =
  match ta input with
  | Ok _ as ok -> ok
  | Error _ -> tb input
;;

let ( >>|= ) t f input =
  let open Or_error.Let_syntax in
  let%bind rest, x = t input in
  match f x with
  | Ok result -> Ok (rest, result)
  | Error _ as error -> error
;;

let tag_error t ~tag input = t input |> Or_error.tag ~tag

(* Combinators *)

let char char = function
  | c :: rest when Char.equal char c -> Ok (rest, c)
  | c ->
    Or_error.error_string
      ("expected next char to be '"
       ^ String.of_char char
       ^ "', got \""
       ^ String.of_char_list c
       ^ "\"")
;;

let string string =
  let chars = String.to_list string in
  List.map ~f:char chars
  |> all
  |> tag_error ~tag:("while trying to parse string \"" ^ string ^ "\": ")
  >>| String.of_char_list
;;

let predicate ~f input =
  let parsed, rest = List.split_while ~f input in
  Ok (rest, String.of_char_list parsed)
;;

let consume t = t >>| Fn.const ()
let consume_char c = consume (char c)
let consume_line = consume (char '\n')
let line t = t <* consume_line
let whitespace = consume (predicate ~f:Char.is_whitespace)
let until_whitespace = predicate ~f:(Fn.non Char.is_whitespace) <* whitespace
let upto_newline = line (predicate ~f:(Fn.non (Char.equal '\n')))

let int input =
  let positive_int =
    predicate ~f:Char.is_digit
    >>|= function
    | "" -> Or_error.error_string "empty string of digits"
    | s -> Ok (Int.of_string s)
  in
  match input with
  | '-' :: rest -> (Int.neg <$> positive_int) rest
  | chars -> positive_int chars
;;

let rec any t input =
  let open Or_error.Let_syntax in
  match t input with
  | Ok (rest, x) ->
    (match rest with
     | [] -> Ok (rest, [ x ])
     | _ ->
       let%map rest', xs = any t rest in
       rest', x :: xs)
  | Error _ -> Ok (input, [])
;;

let some t =
  let f x xs = x :: xs in
  f <$> t <*> any t
;;

let list t ~sep =
  let f xs x = xs @ [ x ] in
  f <$> any (t <* consume sep) <*> t <|> return []
;;

let run t s = t (String.to_list s) |> Or_error.map ~f:snd
let run_exn t s = run t s |> Or_error.ok_exn

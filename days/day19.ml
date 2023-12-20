open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Category = struct
  type t =
    | X
    | M
    | A
    | S
  [@@deriving equal, sexp_of]

  let parse = function
    | 'x' :: rest -> Ok (rest, X)
    | 'm' :: rest -> Ok (rest, M)
    | 'a' :: rest -> Ok (rest, A)
    | 's' :: rest -> Ok (rest, S)
    | _ -> Or_error.error_string "invalid category"
  ;;
end

module Part = struct
  type t =
    { x : int
    ; m : int
    ; a : int
    ; s : int
    }

  let get { x; m; a; s } = function
    | Category.X -> x
    | M -> m
    | A -> a
    | S -> s
  ;;

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    let%map () = consume_char '{'
    and x = consume (string "x=") *> int <* consume_char ','
    and m = consume (string "m=") *> int <* consume_char ','
    and a = consume (string "a=") *> int <* consume_char ','
    and s = consume (string "s=") *> int <* consume_char '}' in
    { x; m; a; s }
  ;;
end

module Rule = struct
  type t =
    { category : Category.t
    ; comparison : [ `Less | `Greater ]
    ; threshold : int
    ; next_workflow : string
    }

  let matches { category; comparison; threshold; _ } part =
    let value = Part.get part category in
    match comparison with
    | `Less -> value < threshold
    | `Greater -> value > threshold
  ;;

  let invert { category; comparison; threshold; next_workflow } =
    match comparison with
    | `Less ->
      { category; comparison = `Greater; threshold = threshold - 1; next_workflow }
    | `Greater ->
      { category; comparison = `Less; threshold = threshold + 1; next_workflow }
  ;;

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    let%map category = Category.parse
    and comparison_char = char '<' <|> char '>'
    and threshold = int <* consume_char ':'
    and next_workflow = predicate ~f:Char.is_alpha in
    let comparison = if Char.equal comparison_char '<' then `Less else `Greater in
    { category; comparison; threshold; next_workflow }
  ;;
end

module Workflow = struct
  type t =
    { rules : Rule.t list
    ; fallback : string
    }

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    let%map name = to_char '{'
    and rules = some (Rule.parse <* consume_char ',')
    and fallback = to_char '}' in
    name, { rules; fallback }
  ;;
end

let parse_workflows_and_parts =
  let open Adventofcode.Parser.Let_syntax in
  let%map workflows = some (line Workflow.parse)
  and () = consume_line
  and parts = some (line Part.parse) in
  Hashtbl.of_alist_exn (module String) workflows, parts
;;

let rec is_accepted workflows workflow_name part =
  match workflow_name with
  | "A" -> true
  | "R" -> false
  | _ ->
    let { Workflow.rules; fallback } = Hashtbl.find_exn workflows workflow_name in
    let matching_rule = List.find rules ~f:(fun r -> Rule.matches r part) in
    (match matching_rule with
     | Some { Rule.next_workflow; _ } -> is_accepted workflows next_workflow part
     | None -> is_accepted workflows fallback part)
;;

module Rating_ranges = struct
  type t = (Category.t, int * int) List.Assoc.t

  let apply_rule (t : t) { Rule.category; comparison; threshold; _ } =
    let lower, upper = List.Assoc.find_exn t category ~equal:Category.equal in
    let lower', upper' =
      match comparison with
      | `Less -> lower, Int.min upper threshold
      | `Greater -> Int.max lower threshold, upper
    in
    match lower' + 1 < upper' with
    | false -> None
    | _ ->
      let removed = List.Assoc.remove t category ~equal:Category.equal in
      let new_ranges =
        List.Assoc.add removed category (lower', upper') ~equal:Category.equal
      in
      Some new_ranges
  ;;

  let rec apply_rules t = function
    | [] -> Some t
    | rule :: rest ->
      let%bind.Option t' = apply_rule t rule in
      apply_rules t' rest
  ;;

  let size t =
    List.map t ~f:(fun (_, (lower, upper)) -> upper - lower - 1)
    |> List.reduce_exn ~f:( * )
  ;;

  let default =
    [ Category.X, (0, 4001)
    ; Category.M, (0, 4001)
    ; Category.A, (0, 4001)
    ; Category.S, (0, 4001)
    ]
  ;;
end

let rec find_accepted_from_rules workflows ranges = function
  | [] -> []
  | rule :: rest ->
    (* Apply the current rule to the ranges and recursively find the accepting list of
       ranges *)
    let accepted_from_rule =
      match Rating_ranges.apply_rule ranges rule with
      | Some ranges' -> accepting_ranges workflows rule.next_workflow ranges'
      | None -> []
    in
    (* Invert the current rule (because we only consider the remaining rules if this one
       didn't match) and recursively find the accepting list of ranges from the
       remaining rules *)
    let accepted_from_tail =
      match Rating_ranges.apply_rule ranges (Rule.invert rule) with
      | Some ranges' -> find_accepted_from_rules workflows ranges' rest
      | None -> []
    in
    accepted_from_rule @ accepted_from_tail

and accepting_ranges workflows workflow_name ranges =
  match workflow_name with
  | "A" -> [ ranges ]
  | "R" -> []
  | _ ->
    let { Workflow.rules; fallback } = Hashtbl.find_exn workflows workflow_name in
    let accepted_from_rules = find_accepted_from_rules workflows ranges rules in
    let accepted_from_fallback =
      (* Find the list of ranges that would accept if none of the rules were triggered *)
      let inverted_rules = List.map rules ~f:Rule.invert in
      match Rating_ranges.apply_rules ranges inverted_rules with
      | Some ranges' -> accepting_ranges workflows fallback ranges'
      | None -> []
    in
    accepted_from_rules @ accepted_from_fallback
;;

let part_1 workflows parts =
  List.filter parts ~f:(is_accepted workflows "in")
  |> List.map ~f:(fun { Part.x; m; a; s } -> x + m + a + s)
  |> List.reduce_exn ~f:( + )
;;

let part_2 workflows =
  let accepting = accepting_ranges workflows "in" Rating_ranges.default in
  List.map accepting ~f:Rating_ranges.size |> List.reduce_exn ~f:( + )
;;

let () =
  let input = read_input ~day:19 in
  let workflows, parts = run_exn parse_workflows_and_parts input in
  part_1 workflows parts |> Int.to_string |> print_endline;
  part_2 workflows |> Int.to_string |> print_endline
;;

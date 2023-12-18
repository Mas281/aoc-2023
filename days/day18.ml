open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Direction = struct
  type t =
    | N
    | E
    | S
    | W

  let move t (x, y) steps =
    match t with
    | N -> x, y + steps
    | E -> x + steps, y
    | S -> x, y - steps
    | W -> x - steps, y
  ;;

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    match%map to_whitespace with
    | "U" -> N
    | "R" -> E
    | "D" -> S
    | "L" -> W
    | _ -> failwith "invalid direction"
  ;;

  let of_int_char = function
    | '0' -> E
    | '1' -> S
    | '2' -> W
    | '3' -> N
    | _ -> failwith "invalid direction"
  ;;
end

let parse_plan =
  let open Adventofcode.Parser.Let_syntax in
  let instruction =
    let%map direction = Direction.parse
    and steps = int <* whitespace
    and colour = consume (string "(#") *> to_char ')' in
    direction, steps, colour
  in
  some (line instruction)
;;

let find_vertices plan =
  List.fold
    plan
    ~init:((0, 0), [])
    ~f:(fun (pos, vertices) (direction, steps, _) ->
      let pos' = Direction.move direction pos steps in
      pos', pos' :: vertices)
  |> snd
;;

(* Area of a polygon using the shoelace formula *)
let find_area vertices =
  let rotated_vertices = List.tl_exn vertices @ [ List.hd_exn vertices ] in
  let determinant_sum =
    List.zip_exn vertices rotated_vertices
    |> List.map ~f:(fun ((x1, y1), (x2, y2)) -> (x1 * y2) - (x2 * y1))
    |> List.reduce_exn ~f:( + )
  in
  determinant_sum / 2
;;

let count_boundary_points vertices =
  let rotated_vertices = List.tl_exn vertices @ [ List.hd_exn vertices ] in
  List.zip_exn vertices rotated_vertices
  |> List.map ~f:(fun ((x1, y1), (x2, y2)) -> Int.abs (x1 - x2) + Int.abs (y1 - y2))
  |> List.reduce_exn ~f:( + )
;;

let fix_plan =
  List.map ~f:(fun (_, _, colour) ->
    let steps = Int.Hex.of_string ("0x" ^ String.subo colour ~len:5) in
    let direction = Direction.of_int_char colour.[String.length colour - 1] in
    direction, steps, "")
;;

let part_1 plan =
  let vertices = find_vertices plan in
  let area = find_area vertices in
  let boundary_points = count_boundary_points vertices in
  (* Pick's theorem: A = i + b/2 - 1 *)
  let interior_points = area + 1 - (boundary_points / 2) in
  boundary_points + interior_points
;;

let part_2 = Fn.compose part_1 fix_plan

let () =
  let input = read_input ~day:18 in
  let plan = run_exn parse_plan input in
  part_1 plan |> Int.to_string |> print_endline;
  part_2 plan |> Int.to_string |> print_endline
;;

open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_hailstones =
  let open Adventofcode.Parser.Let_syntax in
  let hailstone =
    let%map x = Float.of_int <$> (int <* consume (string ", "))
    and y = Float.of_int <$> (int <* consume (string ", "))
    and z = Float.of_int <$> (int <* consume (string " @ "))
    and vx = Float.of_int <$> (int <* consume (string ", "))
    and vy = Float.of_int <$> (int <* consume (string ", "))
    and vz = Float.of_int <$> int in
    x, y, z, vx, vy, vz
  in
  some (line hailstone)
;;

(* https://en.wikipedia.org/wiki/Line–line_intersection *)
let intersect_2d (x1, y1, _, vx1, vy1, _) (x3, y3, _, vx3, vy3, _) =
  let open Float.O in
  let x2, y2 = x1 + vx1, y1 + vy1 in
  let x4, y4 = x3 + vx3, y3 + vy3 in
  let t =
    (((x1 - x3) * (y3 - y4)) - ((y1 - y3) * (x3 - x4)))
    / (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
  in
  let u =
    (((x1 - x3) * (y1 - y2)) - ((y1 - y3) * (x1 - x2)))
    / (((x1 - x2) * (y3 - y4)) - ((y1 - y2) * (x3 - x4)))
  in
  match t > 0.0 && u > 0.0 with
  | true -> Some (x1 + (t * (x2 - x1)), y1 + (t * (y2 - y1)))
  | _ -> None
;;

(* https://paulbourke.net/geometry/pointlineplane *)
let intersect_3d (x1, y1, z1, vx1, vy1, vz1) (x3, y3, z3, vx3, vy3, vz3) =
  let open Float.O in
  let x2, x4 = x1 + vx1, x3 + vx3 in
  let y2, y4 = y1 + vy1, y3 + vy3 in
  let z2, z4 = z1 + vz1, z3 + vz3 in
  let xs = [| x1; x2; x3; x4 |] in
  let ys = [| y1; y2; y3; y4 |] in
  let zs = [| z1; z2; z3; z4 |] in
  let d m n o p =
    ((xs.(m) - xs.(n)) * (xs.(o) - xs.(p)))
    + ((ys.(m) - ys.(n)) * (ys.(o) - ys.(p)))
    + ((zs.(m) - zs.(n)) * (zs.(o) - zs.(p)))
  in
  let t =
    ((d 0 2 3 2 * d 3 2 1 0) - (d 0 2 1 0 * d 3 2 3 2))
    / ((d 1 0 1 0 * d 3 2 3 2) - (d 3 2 1 0 * d 3 2 1 0))
  in
  let u = (d 0 2 3 2 + (t * d 3 2 1 0)) / d 3 2 3 2 in
  match t > 0.0 && u > 0.0 with
  | true -> Some (x1 + (t * (x2 - x1)), y1 + (t * (y2 - y1)), z1 + (t * (z2 - z1)))
  | _ -> None
;;

let rec count_intersections_in_range ~min ~max = function
  | [] -> 0
  | hailstone :: rest ->
    let intersections =
      let open Float.O in
      List.filter_map rest ~f:(intersect_2d hailstone)
      |> List.filter ~f:(fun (x, y) -> x >= min && x <= max && y >= min && y <= max)
    in
    List.length intersections + count_intersections_in_range rest ~min ~max
;;

let part_1 = count_intersections_in_range ~min:200000000000000.0 ~max:400000000000000.0

let approx_equal a b =
  let open Float.O in
  Float.abs (a - b) <= 1e-9 * Float.max a b
;;

let shared_intersection hailstones =
  let passes_through (px, py, pz) (x, y, z, vx, vy, vz) =
    let open Float.O in
    let t = (px - x) / vx in
    t > 0.0 && approx_equal (y + (t * vy)) py && approx_equal (z + (t * vz)) pz
  in
  let%bind.Option ix, iy, iz =
    intersect_3d (List.hd_exn hailstones) (List.nth_exn hailstones 1)
  in
  match List.for_all hailstones ~f:(passes_through (ix, iy, iz)) with
  | true -> Some (ix, iy, iz)
  | _ -> None
;;

let search_interval = 300

let part_2 hailstones =
  (* Bruteforce the X and Y velocities of the stone until a working combination is found *)
  let xy_range =
    let open Sequence.Let_syntax in
    let%bind x = Sequence.range (-search_interval) search_interval ~stop:`inclusive in
    let%map y = Sequence.range (-search_interval) search_interval ~stop:`inclusive in
    Float.of_int x, Float.of_int y
  in
  let sx, sy =
    let open Float.O in
    Sequence.find_exn xy_range ~f:(fun (sx, sy) ->
      let hailstones' =
        List.map hailstones ~f:(fun (x, y, _, vx, vy, _) ->
          x, y, 0.0, vx - sx, vy - sy, 0.0)
      in
      shared_intersection hailstones' |> Option.is_some)
  in
  (* Bruteforce the Z velocity of the stone and use it to find the starting position *)
  let z_range = Sequence.range (-search_interval) search_interval ~stop:`inclusive in
  let px, py, pz =
    let open Float.O in
    Sequence.find_map z_range ~f:(fun sz ->
      let hailstones' =
        List.map hailstones ~f:(fun (x, y, z, vx, vy, vz) ->
          x, y, z, vx - sx, vy - sy, vz - Float.of_int sz)
      in
      shared_intersection hailstones')
    |> Option.value_exn
  in
  Int.of_float (px +. py +. pz)
;;

let () =
  let input = read_input ~day:24 in
  let hailstones = run_exn parse_hailstones input in
  part_1 hailstones |> Int.to_string |> print_endline;
  part_2 hailstones |> Int.to_string |> print_endline
;;

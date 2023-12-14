open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

(* "Tilt" each line in place so the rounded rocks roll towards the beginning *)
let tilt_to_start lines =
  let tilt_line line =
    let _ =
      Array.foldi line ~init:None ~f:(fun row next_free ->
        function
        | '#' -> None
        | 'O' ->
          (match next_free with
           | None -> None
           | Some free ->
             line.(free) <- 'O';
             line.(row) <- '.';
             Some (free + 1))
        | _ ->
          (match next_free with
           | None -> Some row
           | _ -> next_free))
    in
    ()
  in
  Array.iter lines ~f:tilt_line
;;

let tilt_north grid =
  let columns = Array.transpose_exn grid in
  tilt_to_start columns;
  Array.transpose_exn columns
;;

let tilt_west grid =
  tilt_to_start grid;
  grid
;;

let tilt_south grid =
  let columns = Array.transpose_exn grid in
  Array.iter columns ~f:Array.rev_inplace;
  tilt_to_start columns;
  Array.iter columns ~f:Array.rev_inplace;
  Array.transpose_exn columns
;;

let tilt_east grid =
  Array.iter grid ~f:Array.rev_inplace;
  tilt_to_start grid;
  Array.iter grid ~f:Array.rev_inplace;
  grid
;;

let cycle grid = tilt_north grid |> tilt_west |> tilt_south |> tilt_east

(* Calculate the load on the northern support beams *)
let north_load grid =
  let columns = Array.transpose_exn grid in
  let column_load =
    Array.foldi ~init:0 ~f:(fun row acc ->
      function
      | 'O' ->
        let height = Array.length columns - row in
        acc + height
      | _ -> acc)
  in
  Array.map columns ~f:column_load |> Array.reduce_exn ~f:( + )
;;

let part_1 grid =
  let tilted = tilt_north grid in
  north_load tilted
;;

module State = struct
  type t = string [@@deriving compare, hash, sexp_of]

  let of_grid grid = Array.map grid ~f:String.of_array |> String.concat_array
end

(* If we cycle the grid enough times, it enters a loop. We find the number of cycles
   before the loop is entered, and the length of the loop, and use modular arithmetic to
   work out the minimum number of cycles we need to apply to obtain a grid that is the
   same as if it had been cycled 1 billion times *)
let find_loop grid =
  let seen = Hashtbl.create (module State) in
  Hashtbl.add_exn seen ~key:(State.of_grid grid) ~data:0;
  let steps_before_loop, loop_length =
    Sequence.fold_until
      (Sequence.range 1 (Int.pow 10 9))
      ~init:grid
      ~f:(fun prev_grid iteration ->
        let cycled_grid = cycle prev_grid in
        let state = State.of_grid cycled_grid in
        match Hashtbl.find seen state with
        | Some prev_iteration ->
          (* The grid is the same as one in a previous iteration *)
          let steps_before_loop = prev_iteration in
          let loop_length = iteration - prev_iteration in
          Stop (steps_before_loop, loop_length)
        | None ->
          Hashtbl.add_exn seen ~key:state ~data:iteration;
          Continue cycled_grid)
      ~finish:(fun _ -> failwith "no cycle")
  in
  steps_before_loop, loop_length
;;

let part_2 grid =
  let steps_before_loop, loop_length = find_loop grid in
  let cycles_needed =
    steps_before_loop + ((Int.pow 10 9 - steps_before_loop) % loop_length)
  in
  let final_grid =
    Sequence.fold (Sequence.range 0 cycles_needed) ~init:grid ~f:(fun prev_grid _ ->
      cycle prev_grid)
  in
  north_load final_grid
;;

let () =
  let input = read_input ~day:14 in
  let grid =
    let parse_grid = List.to_array <$> (List.map ~f:String.to_array <$> some to_eol) in
    run_exn parse_grid input
  in
  part_1 grid |> Int.to_string |> print_endline;
  part_2 grid |> Int.to_string |> print_endline
;;

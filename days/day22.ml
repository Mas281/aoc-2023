open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Position = struct
  type t = int * int * int [@@deriving compare, equal, hash, sexp_of]

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    let%map x = int <* consume_char ','
    and y = int <* consume_char ','
    and z = int in
    x, y, z
  ;;
end

module Block = struct
  module T = struct
    type t =
      { id : int
      ; pos1 : Position.t
      ; pos2 : Position.t
      }
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let all_xy { pos1 = x_min, y_min, _; pos2 = x_max, y_max, _; _ } =
    let open Sequence.Let_syntax in
    let%bind x = Sequence.range x_min x_max ~stop:`inclusive in
    let%map y = Sequence.range y_min y_max ~stop:`inclusive in
    x, y
  ;;
end

let parse_blocks =
  let open Adventofcode.Parser.Let_syntax in
  let coordinate_pair =
    let%map pos1 = Position.parse
    and () = consume_char '~'
    and pos2 = Position.parse in
    pos1, pos2
  in
  let%map coordinates = some (line coordinate_pair) in
  List.mapi coordinates ~f:(fun id (pos1, pos2) -> { Block.id; pos1; pos2 })
;;

module Height_map = struct
  module XY = struct
    type t = int * int [@@deriving compare, hash, sexp_of]
  end

  type t = (XY.t, int) Hashtbl.t

  let create () = Hashtbl.create (module XY)
  let find (t : t) (x, y) = Hashtbl.find t (x, y) |> Option.value ~default:0
end

module Block_map = struct
  type t = (Position.t, Block.t) Hashtbl.t

  let create blocks : t =
    let t = Hashtbl.create (module Position) in
    List.iter
      blocks
      ~f:(fun ({ Block.pos1 = _, _, z_min; pos2 = _, _, z_max; _ } as block) ->
        Block.all_xy block
        |> Sequence.iter ~f:(fun (x, y) ->
          Hashtbl.set t ~key:(x, y, z_min) ~data:block;
          Hashtbl.set t ~key:(x, y, z_max) ~data:block));
    t
  ;;

  let blocks_supported_by t ({ Block.pos2 = _, _, z_max; _ } as block) =
    Block.all_xy block
    |> Sequence.filter_map ~f:(fun (x, y) -> Hashtbl.find t (x, y, z_max + 1))
    |> Sequence.to_list
    |> List.dedup_and_sort ~compare:Block.compare
  ;;

  let blocks_supporting t ({ Block.pos1 = _, _, z_min; _ } as block) =
    Block.all_xy block
    |> Sequence.filter_map ~f:(fun (x, y) -> Hashtbl.find t (x, y, z_min - 1))
    |> Sequence.to_list
    |> List.dedup_and_sort ~compare:Block.compare
  ;;
end

let settle_blocks blocks =
  let height_map = Height_map.create () in
  let lowest_first { Block.pos1 = _, _, z1; _ } { Block.pos1 = _, _, z2; _ } =
    Int.compare z1 z2
  in
  List.sort blocks ~compare:lowest_first
  |> List.map ~f:(fun ({ id; pos1 = x1, y1, z1; pos2 = x2, y2, z2 } as block) ->
    let highest_occupied =
      Block.all_xy block
      |> Sequence.map ~f:(Height_map.find height_map)
      |> Sequence.max_elt ~compare:Int.compare
      |> Option.value_exn
    in
    let z1' = highest_occupied + 1 in
    let z2' = z1' + (z2 - z1) in
    Sequence.iter (Block.all_xy block) ~f:(fun xy ->
      Hashtbl.update height_map xy ~f:(Fn.const z2'));
    { Block.id; pos1 = x1, y1, z1'; pos2 = x2, y2, z2' })
;;

let can_disintegrate block_map block =
  (* A block can be disintegrated if all the blocks it supports are supported by more
     than one block *)
  Block_map.blocks_supported_by block_map block
  |> List.for_all ~f:(fun supported ->
    let supports = Block_map.blocks_supporting block_map supported in
    List.length supports > 1)
;;

let chain_reaction_length block_map initial_block =
  (* Essentially a BFS on a DAG, where A->B means that block B rests on top of block A.
     We maintain a set of all the blocks that have fallen so far (including the original
     disintegrated block) and the frontier of the search. In each iteration, blocks that
     are 1) a successor of any block in the frontier and 2) whose predecessors have all
     already fallen are added to the chain. *)
  let rec f chain frontier =
    let new_falling_blocks =
      List.bind frontier ~f:(Block_map.blocks_supported_by block_map)
      |> List.filter ~f:(fun supported ->
        let supports = Block_map.blocks_supporting block_map supported in
        List.for_all supports ~f:(Set.mem chain))
      |> List.dedup_and_sort ~compare:Block.compare
    in
    match new_falling_blocks with
    | [] -> Set.length chain - 1 (* -1 to exclude the original disintegrated block *)
    | _ ->
      let chain' = List.fold new_falling_blocks ~init:chain ~f:Set.add in
      f chain' new_falling_blocks
  in
  f (Set.singleton (module Block) initial_block) [ initial_block ]
;;

let part_1 block_map blocks =
  List.filter blocks ~f:(can_disintegrate block_map) |> List.length
;;

let part_2 block_map blocks =
  List.map blocks ~f:(chain_reaction_length block_map) |> List.reduce_exn ~f:( + )
;;

let () =
  let input = read_input ~day:22 in
  let blocks = run_exn parse_blocks input in
  let settled_blocks = settle_blocks blocks in
  let block_map = Block_map.create settled_blocks in
  part_1 block_map settled_blocks |> Int.to_string |> print_endline;
  part_2 block_map settled_blocks |> Int.to_string |> print_endline
;;

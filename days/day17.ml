open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

let parse_grid =
  let open Adventofcode.Parser.Let_syntax in
  let row =
    let%map chars = String.to_array <$> to_eol in
    Array.map chars ~f:Char.get_digit_exn
  in
  Array.of_list <$> some row
;;

module Direction = struct
  type t =
    | N
    | E
    | S
    | W
  [@@deriving compare, equal, hash, sexp_of]

  let opposite = function
    | N -> S
    | E -> W
    | S -> N
    | W -> E
  ;;

  let move t (row, col) =
    match t with
    | N -> row - 1, col
    | E -> row, col + 1
    | S -> row + 1, col
    | W -> row, col - 1
  ;;

  let ( = ) = equal
  let ( <> ) a b = not (equal a b)
end

module Position = struct
  type t = int * int [@@deriving compare, equal, hash, sexp_of]
end

module State = struct
  type t =
    { position : Position.t
    ; direction : Direction.t
    ; consecutive : int
    }
  [@@deriving compare, hash, sexp_of]
end

module Queue_element = struct
  module T = struct
    type t =
      { cost : int
      ; state : State.t
      }
    [@@deriving sexp_of]

    let compare t other =
      match t.cost = other.cost with
      | true -> State.compare t.state other.state
      | _ -> Int.compare t.cost other.cost
    ;;
  end

  include T
  include Comparator.Make (T)
end

let allowed_directions current consecutive ~min_straight ~max_straight =
  let open Direction in
  List.filter [ N; E; S; W ] ~f:(fun next ->
    current <> opposite next
    && ((next <> current && consecutive >= min_straight)
        || (next = current && consecutive < max_straight)))
;;

let in_bounds grid (row, col) =
  row >= 0 && row < Array.length grid && col >= 0 && col < Array.length grid.(0)
;;

let find_or_default table key =
  Hashtbl.find table key |> Option.value ~default:Int.max_value
;;

let update_queue
  grid
  queue
  min_cost
  ({ cost; state = { position; direction; consecutive } } : Queue_element.t)
  ~min_straight
  ~max_straight
  =
  let next_directions =
    allowed_directions direction consecutive ~min_straight ~max_straight
  in
  let next_states =
    List.map next_directions ~f:(fun dir -> dir, Direction.move dir position)
    |> List.filter ~f:(fun (_, pos) -> in_bounds grid pos)
    |> List.map ~f:(fun (direction', position') ->
      let consecutive' =
        match Direction.equal direction direction' with
        | true -> consecutive + 1
        | _ -> 1
      in
      { State.position = position'; direction = direction'; consecutive = consecutive' })
  in
  let queue' = Set.remove_index queue 0 in
  List.fold
    next_states
    ~init:queue'
    ~f:(fun prev_queue ({ position = position'; _ } as state') ->
      let cost' =
        let row', col' = position' in
        cost + grid.(row').(col')
      in
      match cost' < find_or_default min_cost state' with
      | false -> prev_queue
      | _ ->
        Hashtbl.set min_cost ~key:state' ~data:cost';
        Set.add prev_queue ({ cost = cost'; state = state' } : Queue_element.t))
;;

let dijkstra grid ~min_straight ~max_straight =
  let min_cost = Hashtbl.create (module State) in
  let bottom_right = Array.length grid - 1, Array.length grid.(0) - 1 in
  let rec bfs queue =
    let ({ cost; state = { position; consecutive; _ } } as elem : Queue_element.t) =
      Set.min_elt_exn queue
    in
    match Position.equal position bottom_right && consecutive >= min_straight with
    | true -> cost
    | _ ->
      let queue' = update_queue grid queue min_cost elem ~min_straight ~max_straight in
      bfs queue'
  in
  let queue =
    Set.of_list
      (module Queue_element)
      [ { cost = 0; state = { position = 0, 0; direction = N; consecutive = 0 } }
      ; { cost = 0; state = { position = 0, 0; direction = E; consecutive = 0 } }
      ; { cost = 0; state = { position = 0, 0; direction = S; consecutive = 0 } }
      ; { cost = 0; state = { position = 0, 0; direction = W; consecutive = 0 } }
      ]
  in
  bfs queue
;;

let part_1 = dijkstra ~min_straight:0 ~max_straight:3
let part_2 = dijkstra ~min_straight:4 ~max_straight:10

let () =
  let input = read_input ~day:17 in
  let grid = run_exn parse_grid input in
  part_1 grid |> Int.to_string |> print_endline;
  part_2 grid |> Int.to_string |> print_endline
;;

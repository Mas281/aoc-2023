open! Core
open! Adventofcode.Parser
open! Adventofcode.Utils

module Pulse = struct
  type t =
    | Low
    | High
  [@@deriving equal]
end

module Module_type = struct
  type t =
    | Broadcaster
    | Flip_flop of bool ref
    | Conjunction of (string, Pulse.t) Hashtbl.t

  let parse = function
    | '%' :: rest -> Ok (rest, Flip_flop (ref false))
    | '&' :: rest -> Ok (rest, Conjunction (Hashtbl.create (module String)))
    | chars -> Ok (chars, Broadcaster)
  ;;
end

module Module = struct
  type t =
    { type_ : Module_type.t
    ; receivers : string list
    }

  let register_sender { type_; _ } sender =
    match type_ with
    | Broadcaster | Flip_flop _ -> ()
    | Conjunction memory -> Hashtbl.add_exn memory ~key:sender ~data:Pulse.Low
  ;;

  let receive_pulse { type_; _ } pulse from =
    match type_ with
    | Broadcaster -> failwith "broadcaster should not receive any pulses"
    | Flip_flop enabled ->
      (match pulse with
       | Pulse.High -> None
       | Pulse.Low ->
         enabled := not !enabled;
         (match !enabled with
          | true -> Some Pulse.High
          | _ -> Some Pulse.Low))
    | Conjunction memory ->
      Hashtbl.update memory from ~f:(Fn.const pulse);
      (match Hashtbl.for_all memory ~f:(Pulse.equal Pulse.High) with
       | true -> Some Pulse.Low
       | _ -> Some Pulse.High)
  ;;

  let reset { type_; _ } =
    match type_ with
    | Broadcaster -> ()
    | Flip_flop enabled -> enabled := false
    | Conjunction memory -> Hashtbl.map_inplace memory ~f:(Fn.const Pulse.Low)
  ;;

  let parse =
    let open Adventofcode.Parser.Let_syntax in
    let%map type_ = Module_type.parse
    and name = to_whitespace
    and () = consume (string "->") <* whitespace
    and receivers = list (predicate ~f:Char.is_alpha) ~sep:(string ", ") in
    name, { type_; receivers }
  ;;
end

let parse_modules =
  let open Adventofcode.Parser.Let_syntax in
  let%map modules = some (line Module.parse) in
  Hashtbl.of_alist_exn (module String) modules
;;

let register_module_senders modules =
  Hashtbl.iteri modules ~f:(fun ~key:sender_name ~data:{ Module.receivers; _ } ->
    List.iter receivers ~f:(fun receiver_name ->
      match Hashtbl.find modules receiver_name with
      | Some receiver -> Module.register_sender receiver sender_name
      | None -> ()))
;;

let find_senders_to modules receiver =
  Hashtbl.to_alist modules
  |> List.filter_map ~f:(fun (sender_name, { Module.receivers; _ }) ->
    match List.mem receivers receiver ~equal:String.equal with
    | true -> Some sender_name
    | _ -> None)
;;

(* Simulates the button being pressed once. Each time a new pulse is sent from a module,
   [f] is used to update the initial state provided by [init] *)
let press_button modules ~init ~f =
  let rec bfs state = function
    | [] -> state
    | (sender_name, pulse) :: rest ->
      let open Option.Let_syntax in
      let { Module.receivers; _ } = Hashtbl.find_exn modules sender_name in
      let new_pulses =
        List.filter_map receivers ~f:(fun receiver_name ->
          let%bind receiver = Hashtbl.find modules receiver_name in
          let%map receiver_pulse = Module.receive_pulse receiver pulse sender_name in
          receiver_name, receiver_pulse)
      in
      let state' = f state sender_name pulse receivers in
      bfs state' (rest @ new_pulses)
  in
  bfs init [ "broadcaster", Pulse.Low ]
;;

let part_1 modules =
  let count_pulses () =
    (* We start with 1 low pulse from the button module to the broadcaster module *)
    press_button modules ~init:(1, 0) ~f:(fun (low, high) _ pulse receivers ->
      match pulse with
      | Pulse.Low -> low + List.length receivers, high
      | Pulse.High -> low, high + List.length receivers)
  in
  let low_pulses, high_pulses =
    Sequence.range 0 1000
    |> Sequence.map ~f:(fun _ -> count_pulses ())
    |> Sequence.reduce_exn ~f:(Tuple2.map2 ~f:( + ))
  in
  low_pulses * high_pulses
;;

let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a % b)
;;

let lcm a b = a * b / gcd a b

let part_2 modules =
  (* There is a single module which sends pulses to rx, call this module S. S has several
     modules which send pulses to it, call these modules A_1, ..., A_n. We find how many
     button presses each A_i takes to send a high pulse to S, and assume that this occurs
     on a cycle. We find the LCM of these cycle lengths to determine the number of button
     presses before rx first receives a low pulse *)
  let sender_to_rx = find_senders_to modules "rx" |> List.hd_exn in
  let senders_to_sender_to_rx = find_senders_to modules sender_to_rx in
  let sends_high watching =
    (* Presses the button once and returns whether [watching] sent a high pulse to
       [sender_to_rx] at any point *)
    press_button modules ~init:false ~f:(fun current sender pulse receivers ->
      current
      || (String.equal sender watching
          && Pulse.equal pulse Pulse.High
          && List.mem receivers sender_to_rx ~equal:String.equal))
  in
  let presses_to_send_high sender =
    (* Resets each of the modules, then finds the minimum number of button presses before
       [sender] sends a high pulse to [sender_to_rx] *)
    Hashtbl.iter modules ~f:Module.reset;
    Sequence.range 0 (Int.pow 10 4)
    |> Sequence.fold_until
         ~init:1
         ~f:(fun n _ ->
           match sends_high sender with
           | true -> Stop n
           | false -> Continue (n + 1))
         ~finish:(fun _ -> failwith (sender ^ " did not send a high pulse in time"))
  in
  List.map senders_to_sender_to_rx ~f:presses_to_send_high |> List.reduce_exn ~f:lcm
;;

let () =
  let input = read_input ~day:20 in
  let modules = run_exn parse_modules input in
  register_module_senders modules;
  part_1 modules |> Int.to_string |> print_endline;
  part_2 modules |> Int.to_string |> print_endline
;;

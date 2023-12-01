open! Core

let read_input ~day =
  let day_padded = Int.to_string day |> String.pad_left ~char:'0' ~len:2 in
  let filename = "inputs" ^/ "day" ^ day_padded ^ ".txt" in
  String.strip (In_channel.read_all filename)
;;

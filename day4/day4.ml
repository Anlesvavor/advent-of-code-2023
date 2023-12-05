
(* wtf OCaml, no Int.pow?? *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let iota (n : int) : int list =
  let rec iota' n acc =
    if n = 0
    then 0 :: acc
    else iota' (pred n) (n :: acc)
  in
  iota' n []

let int_of_string' ?(context = "-No Context-") (s : string) =
  s
  |> int_of_string_opt
  |> function
  | Some n -> n
  | None -> failwith @@ Printf.sprintf "Couldn't parse %s Context: %s" s context

let sum = List.fold_left (+) 0

module ScracthCard = struct
  type t =
    { id: int
    ; win_nums : int list
    ; nums : int list
    }

  let from_string (str : string) : t =
    let (id_str, win_nums_str, nums_str) =
      match String.split_on_char ':' str with
      | [] -> failwith @@ Printf.sprintf "Bad format: %s" str
      | id_str :: tl ->
        List.nth tl 0
        |> String.split_on_char '|'
        |> function
        | win_nums_str :: nums_str :: [] -> (id_str, win_nums_str, nums_str)
        | _ -> failwith @@ Printf.sprintf "Bad format: %s" str
    in
    let parse_nums s =
      s
      |> String.trim
      |> String.split_on_char ' '
      |> List.filter_map (int_of_string_opt)
    in
    { id = (id_str |> String.split_on_char ' ' |> List.rev |> List.hd |> int_of_string')
    ; win_nums = (parse_nums win_nums_str)
    ; nums = (parse_nums nums_str)
    }

  let matches (t : t) : int list =
    t.nums
    |> List.filter (fun x -> t.win_nums |> (List.mem x) )

  let points (t : t) : int =
    t
    |> matches
    |> List.length
    |> pred
    |> (Int.shift_left 1)

  let () = assert(4 = points { id = 0; win_nums = [1; 2; 3]; nums = [1; 2; 3]})
  let () = assert(1 = points { id = 0; win_nums = [1; 2; 3]; nums = [3]})

  let redeem_copies (cards : t list) : t list =
    let cards_dict =
      cards
      |> List.map (fun x -> (x.id, x))
    in
    let rec aux (stack : t list) (acc : t list) : t list =
      match stack with
      | [] -> acc
      | hd :: tl ->
        let prize_cards =
          hd
          |> matches
          |> List.length
          |> iota
          |> List.tl
          |> List.map (fun i ->
              let won_card_id = i + hd.id in
              List.assoc won_card_id cards_dict
            )
        in
        let stack' = List.append prize_cards tl in
        aux stack' (hd :: acc)
    in
    aux cards []

end

let main (lines : string list) : int =
  lines
  |> List.map (fun s -> s |> ScracthCard.from_string |> ScracthCard.points )
  |> sum

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day4.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day4.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

(* part two *)

let main_part_two (lines : string list) : int =
  lines
  |> List.map (ScracthCard.from_string)
  |> ScracthCard.redeem_copies
  |> List.length

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day4.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day4part2.example.txt") in *)
  let () = print_string "\n---\n" in
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main_part_two lines)

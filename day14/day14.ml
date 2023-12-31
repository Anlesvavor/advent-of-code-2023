let iota (n : int) : int list =
  let rec iota' n acc =
    if n = 0
    then 0 :: acc
    else iota' (pred n) (n :: acc)
  in
  iota' n []

let list_of_string str =
  str |> String.to_seq |> List.of_seq

let string_to_list (str : string) : char list =
  str
  |> String.fold_left (fun acc x -> x :: acc) []
  |> List.rev

let transpose (list : 'a list list) : 'a list list =
  let initial, tail =
    match (list |> List.rev) with
    | [] -> failwith "Bad format"
    | initial :: tail -> (initial |> List.map (fun x -> [x])), tail
  in
  tail
  |> List.fold_left (fun acc xs -> List.map2 (fun a b -> a :: b) xs acc) initial

let hd_opt =
  function
  | [] -> None
  | a :: _ -> Some a

let split_preserving_on (item : 'a) (list : 'a list) : 'a list list =
  let rec aux acc cur =
    function
    | [] -> cur :: acc
    | hd :: tl ->
      if hd = item
      then aux ((hd :: cur) :: acc) [] tl
      else aux acc (hd :: cur) tl
  in
  aux [] [] list
  |> List.map (List.rev) |> List.rev

let main (lines : string list) =
  lines
  |> List.map string_to_list
  |> transpose
  |> List.map (fun xs ->
      let col = xs |> split_preserving_on '#' in
      let len = List.length xs in
      let rolled_boulders =
        col
        |> List.map (
          List.sort (fun a b ->
              match a, b with
              | 'O', _ -> -1
              | _, 'O' -> 1
              | '#', _ -> 1
              | _, '#' -> -1
              | _, _ -> 0
            )
        )
        |> List.concat
      in
      let score_list = len |> iota |> List.tl |> List.rev in
      let scored_boulders = List.combine rolled_boulders score_list in
      let scores =
        scored_boulders
        |> List.filter_map (function
            | 'O', s -> Some s
            | _, _ -> None
          )
      in
      List.fold_left (+) 0 scores
    )
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day14.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day14.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main lines)
(* main lines |> List.iter (print_endline) *)



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

let destructive_parse_str_num_list (str : string) : int list =
  str
  |> String.split_on_char ' '
  |> List.filter_map (int_of_string_opt)

let main lines =
  let times, distances =
    match lines with
    | times :: distances :: [] -> destructive_parse_str_num_list times, destructive_parse_str_num_list distances
    | _ -> failwith "Bad format" 
  in
  let race t a = a * (t - a) in
  List.combine times distances
  |> List.map (fun (t, d) ->
      let r =
        iota t
        |> List.map (fun a -> race t a)
        |> List.filter ((<) d)
        |> List.length
      in
      r
    )
  |> List.fold_left ( * ) 1

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day6.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day6.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

(* Part two *)

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day6part2.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day6part2.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

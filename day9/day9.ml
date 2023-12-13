let destructive_parse_str_num_list (str : string) : int list =
  str
  |> String.split_on_char ' '
  |> List.filter_map (int_of_string_opt)

let deltas (list : int list) =
  let rec aux acc =
    function
    | [] | [_] -> acc
    | a :: (b :: _ as tl) -> aux ((b - a) :: acc) tl
  in
  aux [] list
  |> List.rev

let deltas' (list : int list) =
  let rec aux acc list =
    if list |> List.for_all ((=) 0)
    then acc
    else
      let delta = deltas list in
      aux (delta :: acc) delta
  in
  aux [] list

let extrapolate_lists lists =
  let rec aux acc =
    function
    | [] | [_] -> acc
    | a :: b :: tl ->
      let a_last = a |> List.rev |> List.hd in
      let b_last = b |> List.rev |> List.hd in
      let b_last' = a_last + b_last in
      aux (b_last' :: acc) ((b @ [b_last']) :: tl)
  in
  aux [] lists

let main lines =
  lines
  |> List.map (destructive_parse_str_num_list)
  |> List.map (fun x -> (deltas' x) @ [x])
  |> List.map (fun xs -> xs |> extrapolate_lists |> List.hd)
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day9.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day9.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

let main_part_2 lines =
  lines
  |> List.map (fun s -> s |> destructive_parse_str_num_list |> List.rev)
  |> List.map (fun x -> (deltas' x) @ [x])
  |> List.map (fun xs -> xs |> (extrapolate_lists ) |> List.hd)
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day9.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day9.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main_part_2 lines)

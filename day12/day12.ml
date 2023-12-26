
let div' a b = (float_of_int a) /. (float_of_int b)

let list_of_string str =
  str |> String.to_seq |> List.of_seq

let int_of_string' ?(context = "-No Context-") (s : string) =
  s
  |> int_of_string_opt
  |> function
  | Some n -> n
  | None -> failwith @@ Printf.sprintf "Couldn't parse %s Context: %s" s context

let string_to_list (str : string) : char list =
  str
  |> String.fold_left (fun acc x -> x :: acc) []
  |> List.rev

let destructive_parse_str_num_list (str : string) : int list =
  str
  |> String.split_on_char ' '
  |> List.filter_map (int_of_string_opt)

let group_items (pred : 'a -> 'a -> bool) (list : 'a list) : 'a list list =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as tl) ->
      if pred a b
      then aux (a :: current) acc tl
      else aux [] ((a :: current) :: acc) tl
  in
  List.rev (aux [] [] list)
  |> List.map (List.rev)

let run_lenght_encode list =
  let rec aux acc =
    function
    | [] -> acc
    | x :: xs ->
      match acc with
      | [] -> aux [(x, 1)] xs
      | (hd_val, hd_count) as hd :: tl ->
        let acc' =
          if hd_val = x
          then (hd_val, (succ hd_count)) :: tl
          else (x, 1) :: hd :: tl
        in
        aux acc' xs
  in
  aux [] list |> List.rev

module Report = struct
  type c = Op | NoOp | Ukn
  type graph = c list
  type elt = (graph * (int list))
  type t = elt list

  let c_from_char =
    function
    | '.' -> Op
    | '#' -> NoOp
    | '?' -> Ukn
    | _ -> failwith "Bad char"

  let from_string (str : string) : elt =
    let graph, hints =
      str
      |> String.split_on_char ' '
      |> function
      | a :: b :: [] ->
        let a = a |> list_of_string |> List.map (c_from_char) in
        let b = b |> String.split_on_char ',' |> List.map (int_of_string) in
        (a, b)
      | _ -> failwith "Bad format"
    in
    (graph, hints)

  let from_lines (lines : string list) =
    lines |> List.map (from_string)

  let graph_to_string (graph : graph) : string =
    let c_to_char = function
      | NoOp -> '#'
      | Op -> '.'
      | Ukn -> '?'
    in
    graph
    |> List.map (c_to_char)
    |> List.to_seq
    |> String.of_seq

  (* TODO: Heuristics *)
  let is_valid (elt : elt) : bool =
    let graph, hints = elt in
    graph
    |> run_lenght_encode
    |> List.filter_map (fun (c, count) -> if c = NoOp then (Some count) else None)
    |> (=) hints

  let combinations_of (elt : elt) : t =
    let graph, hints = elt in
    let rec aux acc list =
      match list with
      | [] -> acc
      | Ukn :: tl ->
        let acc' =
          acc
          |> List.fold_left (fun xs x ->
              let x1 = Op :: x in
              let x2 = NoOp :: x in
              let xs1 = x1 :: xs in
              let xs2 = x2 :: xs1 in
              xs2
            ) []
        in
        aux acc' tl
      | hd :: tl -> aux (acc |> List.map (fun xs -> hd :: xs)) tl
    in
    aux [[]] graph
    |> List.map (fun x -> (List.rev x, hints))

end

(*  "???.### 1,1,3" |> Report.from_string |> Report.f;; *)

let main lines =
  let reports =
    lines
    |> Report.from_lines
  in
  reports
  |> List.map (fun x ->
      Report.combinations_of x
      |> List.filter (Report.is_valid)
      |> List.length
    )
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day12.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day12.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main lines)
(* main lines |> List.iter (print_endline) *)

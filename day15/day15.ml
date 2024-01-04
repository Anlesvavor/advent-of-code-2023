let list_of_string str =
  str |> String.to_seq |> List.of_seq

let hash str =
  str
  |> String.to_seq
  |> Seq.fold_left (fun acc c ->
      acc
      |> ((+) (Char.code c))
      |> (( * ) 17)
      |> fun x -> x mod 256
    ) 0

let main (lines : string list) =
  let line = List.hd lines in
  let strings = String.split_on_char ',' line in
  List.fold_left (fun acc s -> acc + (hash s)) 0 strings

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day15.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day15.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main lines)


let split_preserving_on (item : 'a) (list : 'a list) : 'a list list =
  let rec aux acc cur =
    function
    | [] -> cur :: acc
    | hd :: tl ->
      if hd = item
      then aux ([hd] :: cur :: acc) [] tl
      else aux acc (hd :: cur) tl
  in
  aux [] [] list
  |> List.map (List.rev) |> List.rev

module Lens = struct
  type op = Add of int | Remove

  type t = string * op

  let lens_of_string (str : string) : t =
    let rec aux acc =
      function
      | [] -> failwith "Something gone wrong"
      | c :: cs ->
        match c with
        | '=' -> acc, Add (cs |> List.to_seq |> String.of_seq |> int_of_string)
        | '-' -> acc, Remove
        | c -> aux (acc @ [c]) cs
    in
    let label_list, op =
      str
      |> list_of_string
      |> aux []
    in
    (label_list |> List.to_seq |> String.of_seq), op

end

let upsert (f : 'a -> 'b) value list =
  if List.exists (fun x -> (f x) = (f value)) list
  then List.map (fun x -> if (f x) = (f value) then value else x) list
  else list @ [value]

let main_part_2 (lines : string list) =
  let line = List.hd lines in
  let strings = String.split_on_char ',' line in
  let lenses = strings |> List.map Lens.lens_of_string in
  lenses
  |> List.fold_left (fun boxes lens ->
      let (label, op) = lens in
      let lens_hash = hash label in
      let boxes' = boxes |> List.map (fun (id, box) ->
          if id = lens_hash
          then (id, box)
          else (id, match op with
            | Lens.Remove -> box |> List.filter (fun (str, _) -> str = label)
            | Lens.Add focal_lenght -> box |> upsert (fst) (label, focal_lenght)
            )
        )
      in
      boxes'
    ) []
  |> List.fold_left (fun acc (id, boxes) ->
      boxes
      |> List.mapi (fun i (_, focal_lenght) -> (i +1) * focal_lenght)
      |> List.fold_left (+) 0
      |> ((+) acc)
    ) 0

let () =
  (* let lines = In_channel.input_lines (In_channel.open_text  "day15.txt") in *)
  let lines = In_channel.input_lines (In_channel.open_text "day15.example.txt") in
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main_part_2 lines)

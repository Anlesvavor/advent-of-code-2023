
let div' a b = (float_of_int a) /. (float_of_int b)

let list_of_string str =
  str |> String.to_seq |> List.of_seq

let take_if (predicate : 'a -> bool) (a : 'a) : 'a option=
  if predicate a then Some a else None

let take_if_opt (predicate : 'a -> bool) (a : 'a option) : 'a option=
  Option.bind a (fun a -> if predicate a then Some a else None)

(* Pipe Bind *)
let ( |*> ) a f = Option.bind a f

let also e a = let () = e a in a

let pairs list =
  let rec aux acc =
    function
    | [] -> acc
    | x :: xs ->
      let acc' = acc @ List.map (fun y -> (x,y)) xs in
      aux acc' xs
  in
  aux [] list

let manhattan_distance a b =
  let (ax, ay) = a in
  let (bx, by) = b in
  (abs (bx - ax)) + (abs (by - ay))

let zip f list =
  let rec aux acc =
    function
    | [] -> acc
    | (a, b) :: tl -> aux ((f a b) :: acc) tl
  in
  aux [] list

module Star = struct
  type t = int * int

  let from_string (row : int) (str : string) : t list =
    let from_char c =
      if c = '#' then Some c else None
    in
    str |> list_of_string |> List.mapi (fun i c -> (i, row), (from_char c))
    |> List.filter_map (fun (t, c) -> c |*> (Fun.const (Some t)))

  let from_lines (lines : string list) : t list =
    lines |> List.mapi (from_string) |> List.flatten

  let expand_field ?(arg = 2) (starfield : t list) : t list =
    let expand arr =
      let rec aux acc =
        function
        | [] -> acc
        | [x] -> x :: acc
        | x :: (y :: _ as xs) ->
          let delta x' =
            abs (y - x)
            |> function
            | 0 | 1 -> x'
            | diff -> x' + ((diff - 1) * arg) - (diff - 1)
          in
          aux (x :: acc) (List.map (delta) xs)
      in
      aux [] arr
      |> List.rev
    in
    let x_proj, y_proj = starfield |> List.split in
    let y_proj' = y_proj |> expand in
    List.combine x_proj y_proj'
    |> List.mapi (fun i x -> (i,x))
    |> List.sort (fun (_,(x1,_)) (_,(x2,_)) -> compare x1 x2)
    |> (fun l ->
        let i_list, t_list = List.split l in
        let x_list, y_list = List.split t_list in
        let x_list = expand x_list in
        let t_list' = List.combine x_list y_list in
        List.combine i_list t_list'
      )
    |> List.sort (fun (i1,(_,_)) (i2,(_,_)) -> compare i1 i2)
    |> List.map (snd)

end

let main lines =
  let stars =
    lines
    |> Star.from_lines
  in
  stars
  (* |> also (List.iter (fun (a,b)-> Printf.printf "%d,%d; " a b )) *)
  (* |> also (fun _ -> print_newline() ) *)
  |> Star.expand_field
  (* |> also (List.iter (fun (a,b)-> Printf.printf "%d,%d; " a b )) *)
  |> pairs
  |> zip (manhattan_distance)
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day11.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day11.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main lines)
(* main lines |> List.iter (print_endline) *)

let main_part_2 lines =
  let stars =
    lines
    |> Star.from_lines
  in
  stars
  (* |> also (fun _ -> print_newline() ) *)
  (* |> also (List.iter (fun (a,b)-> Printf.printf "%d,%d; " a b )) *)
  (* |> also (fun _ -> print_newline() ) *)
  |> Star.expand_field ~arg:1000000
  (* |> also (List.iter (fun (a,b)-> Printf.printf "%d,%d; " a b )) *)
  |> pairs
  |> zip (manhattan_distance)
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day11.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day11.example.txt") in *)
  (* let () = print_newline () in *)
  (* let () = List.iter (Printf.printf "%s\n") lines in *)
  let () = print_newline () in
  print_int (main_part_2 lines)
(* main lines |> List.iter (print_endline) *)

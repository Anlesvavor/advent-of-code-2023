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

let ( |? ) a f = Option.bind a f

let rec is_sub_list (a : 'a list) (b : 'a list) : bool =
  match a, b with
  | [], [] -> true
  | x :: xs, [] -> true
  | [], y :: ys -> true
  | x :: xs, y :: ys -> if x = y then is_sub_list xs ys else false

let rec is_reflection ?(predicate = (=)) (a : 'a list) (b : 'a list) : bool =
  match a, b with
  | [], [] -> false
  | x :: xs, [] -> false
  | [], y :: ys -> false
  | x :: xs, [y] -> x = y
  | [x], y :: ys -> x = y
  | x :: xs, y :: ys -> if predicate x y then is_reflection xs ys else false

let common_items (a : 'a list) (b : 'a list) : 'a list =
  a |> List.filter ((Fun.flip List.mem) b)

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

let partition_at (position : int) (list : 'a list) : 'a list * 'a list =
  let rec aux acc count =
    function
    | [] -> (acc, [])
    | (x :: xs as list) ->
      if count <= 0
      then (acc |> List.rev, list)
      else aux (x :: acc) (pred count) xs
  in
  aux [] position list

(* partition_at 0 ['a'; 'b'; 'c'; 'd'];; *)
(* partition_at 2 ['a'; 'b'; 'c'; 'd'];; *)
(* partition_at 4 ['a'; 'b'; 'c'; 'd'];; *)
(* partition_at 9 ['a'; 'b'; 'c'; 'd'];; *)

let count_differences (a : 'a list) (b : 'a list) : int =
  List.fold_left2 (fun acc a b -> acc + if a = b then 0 else 1) 0 a b

(* count_differences ['a'; 'b'; 'c'; 'd'] ['a'; 'b'; 'c'; 'd'];; *)

let take (n : int) (list : 'a list) : 'a list =
  let rec aux acc count =
    function
    | [] -> acc
    | x :: xs ->
      if count > n
      then acc
      else aux (x :: acc) (succ count) xs
  in
  aux [] 1 list
  |> List.rev

(* take 2 ['a'; 'b'; 'c'; 'd'] *)

let take_last (n : int) (list : 'a list) : 'a list =
  take n (list |> List.rev) |> List.rev

(* take_last 2 ['a'; 'b'; 'c'; 'd'; 'e'] *)

let reflection_errors_at (reflection_position : int) (list : 'a list) =
  let original, reflection = partition_at reflection_position list in
  let min_len = min (List.length original) (List.length reflection) in
  let original' = take_last min_len original in
  let reflection' = take min_len reflection in
  List.fold_left2 (fun acc a b ->
      acc + count_differences a b
    ) 0 original' (List.rev reflection')

(* reflection_errors_at 2 ['a'; 'b'; 'b'; 'a'] *)
(* reflection_errors_at 2 ['a'; 'b'; 'a'; 'c'] *)

module Image = struct
  type t = char list list
  type rflct = Col of int | Row of int | NoRflct

  let reflect_idxs (elt : char list) : int list =
    let rec aux acc_reflection acc_idx curr_idx =
      function
      | [] -> acc_idx
      | (hd :: tl as list) ->
        if is_reflection list acc_reflection
        then aux (hd :: acc_reflection) (curr_idx :: acc_idx) (succ curr_idx) tl
        else aux (hd :: acc_reflection) acc_idx (succ curr_idx) tl
    in
    aux [] [] 0 elt
    |> List.rev

  let reflect_idxs_of_t (t : t) : int option =
    let initial, tail =
      match t with
      | [] -> failwith "Bad format"
      | initial :: tail -> (reflect_idxs initial), tail
    in
    tail
    |> List.fold_left (fun acc x -> common_items acc (reflect_idxs x)) initial
    |> hd_opt

  let reflections (t : t) : rflct =
    t
    |> reflect_idxs_of_t
    |> function
    | Some a -> Col a
    | None ->
      t
      |> transpose
      |> reflect_idxs_of_t
      |> function
      | Some a -> Row a
      | None -> NoRflct

  let reflection_with_smudges (t : t) : rflct =
    let scan t =
      iota (t |> List.length)
      |> List.filter (fun i -> 1 = reflection_errors_at i t)
      |> Fun.flip (List.nth_opt) 0
    in
    let row_index = scan t in
    match row_index with
    | Some a -> Row a
    | None ->
      let col_index = scan (transpose t) in
      match col_index with
      | Some b -> Col b
      | None -> NoRflct

end

let main (lines : string list) =
  lines
  |> group_items (fun a b -> a <> "" && b <> "")
  |> List.filter_map (fun s ->
      if s <> [""]
      then Some (s |> List.map (string_to_list))
      else None
    )
  |> List.map (Image.reflections)
  |> List.fold_left (fun acc r ->
      acc + match r with
      | Image.Col a -> a
      | Image.Row a -> a * 100
      | Image.NoRflct -> failwith "No reflection?!?"
    ) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day13.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day13.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main lines)
(* main lines |> List.iter (print_endline) *)

let main_part_2 (lines : string list) =
  lines
  |> group_items (fun a b -> a <> "" && b <> "")
  |> List.filter_map (fun s ->
      if s <> [""]
      then Some (s |> List.map (string_to_list))
      else None
    )
  |> List.map (Image.reflection_with_smudges)
  |> List.fold_left (fun acc r ->
      acc + match r with
      | Image.Col a -> a
      | Image.Row a -> a * 100
      | Image.NoRflct -> failwith "No reflection?!?"
    ) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day13.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day13.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main_part_2 lines)
(* main lines |> List.iter (print_endline) *)

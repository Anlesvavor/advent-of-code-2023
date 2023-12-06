
let assert_that ?(message = "") expected actual =
  if expected = actual
  then ()
  else failwith @@ Printf.sprintf "Assertion failed %s" message

let iota (n : int) : int list =
  let rec iota' n acc =
    if n = 0
    then 0 :: acc
    else iota' (pred n) (n :: acc)
  in
  iota' n []

let flip f a b = f b a

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

module Range = struct
  type t = { dest: int; src: int; len: int }

  let range (start : int) (len : int) : int list =
    let upper_bound = start + (pred len) in
    let rec range' acc start =
      if start <= upper_bound
      then
        let start' = succ start in
        range' (start :: acc) start'
      else acc
    in
    range' [] start

  let () =
    let actual = range 10 3 in
    let actual_str =
      actual |> List.fold_left (fun acc a -> acc^Printf.sprintf "%d; " a) ""
    in
    assert_that ~message:actual_str [12; 11; 10] actual

  let from_string (str : string) : t =
    destructive_parse_str_num_list str
    |> function
    | dest :: src :: len :: [] -> { dest; src; len }
    | _ -> failwith @@ Printf.sprintf "Bad format: %s" str

  let compute (t : t) : (int * int) list =
    let dest_range = range t.dest t.len in
    let src_range = range t.src t.len in
    List.combine src_range dest_range

  (* let assoc' (src : int) (t : t) : int option = *)
  (*   List.assoc_opt src (compute t) *)

  let assoc (x : int) (t : t) : int option =
    let upper_bound = t.src + t.len in
    let diff = x - t.src in
    if t.src <= x && x < upper_bound
    then (Some (t.dest + diff))
    else None

end

module Category = struct
  type t = Range.t list

  let from_string (str : string list) : t =
    str
    |> List.map (Range.from_string)

  let assoc (src : int) (category : t) : int =
    category
    |> List.map (Range.assoc src)
    |> List.find_opt (Option.is_some)
    |> function
    | Some (Some dest) -> dest
    | _ -> src

end

let main (lines : string list) =
  let (seeds, categories) =
    match lines with
    | [] -> failwith "Bad format"
    | seed_str :: body_str ->
      let seeds = destructive_parse_str_num_list seed_str in
      let parse_categories (strs : string list) : Category.t list =
        let rec aux acc curr list =
          match list with
          | [] -> acc
          | s :: ss ->
            if String.ends_with ~suffix:"map:" s
            then aux (curr :: acc) [] ss
            else aux acc (s :: curr) ss
        in
        strs
        |> List.filter (fun s -> not(s = ""))
        |> List.rev (* due to folding*)
        |> aux [] []
        |> List.map (Category.from_string)
      in
      (seeds, parse_categories body_str)
  in
  let almanac =
    categories
    |> (flip @@ List.fold_left (fun acc (cat : Category.t) -> Category.assoc acc cat))
  in
  seeds
  |> List.map (almanac)
  |> List.fold_left (Int.min) Int.max_int


let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day5.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day5.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

(* (\* part two *\) *)

(* let main_part_two (lines : string list) : int = *)
(*   lines *)
(*   |> List.map (ScracthCard.from_string) *)
(*   |> ScracthCard.redeem_copies *)
(*   |> List.length *)

(* let () = *)
(*   let lines = In_channel.input_lines (In_channel.open_text  "day4.txt") in *)
(*   (\* let lines = In_channel.input_lines (In_channel.open_text "day4part2.example.txt") in *\) *)
(*   let () = print_string "\n---\n" in *)
(*   let () = List.iter (Printf.printf "%s\n") lines in *)
(*   print_int (main_part_two lines) *)

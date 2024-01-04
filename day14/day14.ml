let iota (n : int) : int list =
  let rec iota' n acc =
    if n = 0
    then 0 :: acc
    else iota' (pred n) (n :: acc)
  in
  iota' n []

module Memoed = struct
  type 'a t = Novel of 'a | Old of 'a

  let novel a = (Novel a)

  let old a = (Old a)

  let memo f =
    let h = Hashtbl.create 11 in
    fun x ->
      try old (Hashtbl.find h x)
      with Not_found ->
        let y = f x in
        Hashtbl.add h x y;
        novel y
end

let get_loop f x =
  let f' = Memoed.memo f in
  let f'' = Memoed.memo f in
  let rec aux cur idx =
    match f' cur with
    | Memoed.Old _ -> cur, idx
    | Memoed.Novel next -> aux next (succ idx)
  in
  let loop_start, loop_start_idx = aux x 0 in
  let rec aux' acc cur =
    match f'' cur with
    | Memoed.Old _ -> acc
    | Memoed.Novel next -> aux' (cur :: acc) next
  in
  let loop = aux' [] loop_start |> List.rev in
  loop, loop_start_idx

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

let score (boulders : char list) : int =
  let score_list = boulders |> List.length |> iota |> List.tl |> List.rev in
  let scored_boulders = List.combine boulders score_list in
  scored_boulders
  |> List.filter_map (function
      | 'O', s -> Some s
      | _, _ -> None
    )
  |> List.fold_left (+) 0

type ord = L | R

let roll_list (ord : ord) (list : char list)  =
  let ordering_f =
    function
    | L -> begin
        let f a b = match a, b with
          | 'O', _ -> -1
          | _, 'O' -> 1
          | _, _ -> 0
        in
        f
      end
    | R -> begin
        let f a b = match a, b with
          | 'O', _ -> 1
          | _, 'O' -> -1
          | _, _ -> 0
        in
        f
      end
  in
  let sections = list |> split_preserving_on '#' in
  sections
  |> List.map (List.sort (ordering_f ord))
  |> List.concat

let cycle lines =
  lines
  |> transpose
  |> List.map (roll_list L)
  |> transpose
  |> List.map (roll_list L)
  |> transpose
  |> List.map (roll_list R)
  |> transpose
  |> List.map (roll_list R)

let print_board (css : char list list) =
  css |> List.iter (fun cs ->
      cs |> List.iter (print_char);
      print_newline ()
    )

let main_part_2 (lines : string list) =
  let charss = (lines |> List.map string_to_list) in
  let it = 1000000000 in
  let loop, idx = get_loop cycle charss in
  List.nth loop ((it - idx) mod (List.length loop))
  |> transpose
  |> List.map score
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day14.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day14.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  let result = main_part_2 lines in
  print_int result
(* print_int (main_part_2 lines) *)
(* main lines |> List.iter (print_endline) *)

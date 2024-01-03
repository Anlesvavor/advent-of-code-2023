let iota (n : int) : int list =
  let rec iota' n acc =
    if n = 0
    then 0 :: acc
    else iota' (pred n) (n :: acc)
  in
  iota' n []

let do_times (n : int) f x =
  let rec aux (n : int) acc =
    if n = 0
    then acc
    else aux (pred n) (f acc)
  in
  aux n x

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

let do_times_until_bored (n : int) f x =
  let f' = Memoed.memo f in
  let rec aux (n : int) acc =
    if n = 0
    then acc, 0
    else
      match f' acc with
      | Memoed.Old acc' ->
        let () = Printf.printf "old: c:%d\n" n in
        acc', n
      | Memoed.Novel acc' ->
        let () = Printf.printf "nvel: c:%d\n" n in
        aux (pred n) acc'
  in
  let result, remained_n = aux n x in
  result, n - remained_n
;; (* For some reason this is needed, weird *)

let do_times_until_loop_with_loop (n : int) f x =
  (* Get to loop start *)
  let x', loop_n_start = do_times_until_bored n f x in
  let f' = Memoed.memo f in
  let rec aux (n : int) acc cur =
    if n = 0
    then cur, acc
    else
      match f' cur with
      | Memoed.Old next ->
        let () = Printf.printf "old: c:%d\n" n in
        next, cur :: acc
      | Memoed.Novel next ->
        let () = Printf.printf "nvel: c:%d\n" n in
        aux (pred n) (cur :: acc) next
  in
  let result, loop = aux n [] x' in
  result, loop_n_start, loop
;;

do_times_until_bored Int.max_int (fun x -> x mod 10) 0

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
  let result, loop_start, loop = do_times_until_loop_with_loop it cycle charss in
  List.nth loop ((it - (it - loop_start-1) ) mod (List.length loop -1) )
  |> transpose
  |> List.map score
  |> List.fold_left (+) 0

(* let result', n', loop' = do_times_until_loop_with_loop 10000 cycle result in *)
(* result::result'::(loop @ loop') *)
(* |> List.map (fun xs -> *)
(*     xs |> transpose |> List.map score *)
(*     |> List.fold_left (+) 0 *)
(*   ) *)
(* do_times_until_bored 1000000000 cycle charss *)
(* |> transpose *)
(* |> List.map (roll_list L) *)
(* |> List.map score *)
(* |> List.fold_left (+) 0 *)

let () =
  (* let lines = In_channel.input_lines (In_channel.open_text  "day14.txt") in *)
  let lines = In_channel.input_lines (In_channel.open_text "day14.example.txt") in
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  let result = main_part_2 lines in
  print_int result
(* print_int (main_part_2 lines) *)
(* main lines |> List.iter (print_endline) *)

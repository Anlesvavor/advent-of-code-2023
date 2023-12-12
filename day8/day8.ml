
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

let is_uppercase c = c >= 'A' && 'Z' >= c

let is_alpha c = ('A' <= c && c <= 'z') || ('0' <= c && c <= '9')

let last_char str =
  str |> String.to_seq |> List.of_seq |> List.rev |> List.hd

module DessertMap = struct
  type elt = (string * (string * string))
  type t = elt list

  let from_string (str : string) : elt =
    let label, left, right =
      let tokens =
        String.fold_right (List.cons) str []
        |> group_items (fun a b -> is_alpha a && is_alpha b)
        |> List.map (fun cs -> cs |> List.map (String.make 1) |> String.concat "")
      in
      List.nth tokens 0, List.nth tokens 5, List.nth tokens 8
    in
    (label, (left, right))

  let from_lines (lines : string list) : t =
    lines |> List.map (from_string)

  let to_string (t : elt) : string =
    let (k,(l,r)) = t in
    Printf.sprintf "%s=%s,%s" k l r

  (* let seq_of_dir_to_dessert_path (t : t) (seq : (`Left | `Right) Seq.t) : t = *)
  (*   match seq () with *)
  (*   | Seq.Cons ((label, ), next) *)
end

let rec map_with_previous f prev (seq : 'a Seq.t) () =
  match seq () with
  | Seq.Nil -> Seq.Nil
  | Seq.Cons (x, xs) ->
    let (x', prev) = f prev x in
    Seq.Cons ((x', prev), map_with_previous f x' xs)

let main lines =
  let steps, dessert_map =
    match lines with
    | steps :: "" :: lines -> steps, (DessertMap.from_lines lines)
    | _ -> failwith "Bad format"
  in
  (* let count = ref 0 in *)
  (* let current = ref (List.find (fun (label, _) -> label = "AAA") dessert_map) in *)
  (* let steps = *)
  (*   steps *)
  (*   |> String.to_seq *)
  (*   |> Seq.map (function 'L' -> `Left | 'R' -> `Right | _ -> failwith "bad format") *)
  (*   |> Array.of_seq *)
  (* in *)
  (* let () = dessert_map |> List.iter (fun x -> print_endline (DessertMap.to_string x)) in *)
  steps
  |> String.to_seq
  |> Seq.map (function 'L' -> `Left | 'R' -> `Right | _ -> failwith "bad format")
  |> Seq.cycle
  |> map_with_previous (fun (label, (left, right)) x ->
      match x with
      | `Left -> (left, (List.assoc left dessert_map)), (left, (List.assoc left dessert_map))
      | `Right -> (right, (List.assoc right dessert_map)), (right, (List.assoc right dessert_map))
    ) (List.find (fun (label, _) -> label = "ZZZ") dessert_map)
  |> Seq.take_while (fun (_, (label, _)) -> label <> "ZZZ")
  (* |> Seq.map (fun (x,p) -> let () = Printf.printf "%s ; %s\n" (DessertMap.to_string x) (DessertMap.to_string p) in x ) *)
  |> Seq.length
  |> succ
(* let next_step = ref (Array.get steps 0) in *)
(* let ddone = ref true in *)
(* while !ddone do *)
(*   print_endline (DessertMap.to_string !current); *)
(*   let label, (left, right) = !current in *)
(*   ddone := if label = "ZZZ" then false else true; *)
(*   print_int !count; *)
(*   print_newline (); *)
(*   next_step := Array.get steps (!count mod (Array.length steps)); *)
(*   count := !count + 1; *)
(*   current := match !next_step with *)
(*     | `Left -> (left, (List.assoc left dessert_map)) *)
(*     | `Right -> (right, (List.assoc right dessert_map)) *)
(* done; *)
(* !count *)

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day8.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day8.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

let main_part_2 lines =
  let steps, dessert_map =
    match lines with
    | steps :: "" :: lines -> steps, (DessertMap.from_lines lines)
    | _ -> failwith "Bad format"
  in
  dessert_map |> List.iter ( fun x ->
      Printf.printf ">%s\n" (DessertMap.to_string x)
    )
  ;
  let ghost_map_start =
    dessert_map
    |> List.filter (fun (label, _) ->
        (last_char label) = 'A'
      )
  in
  let ghost_map_end =
    dessert_map
    |> List.filter (fun (label, _) -> (last_char label) = 'Z')
  in
  ghost_map_start |> List.iter (fun x ->
      Printf.printf "start->%s\n" (DessertMap.to_string x)
    );
  ghost_map_end |> List.iter (fun x ->
      Printf.printf "end->%s\n" (DessertMap.to_string x)
    );
  steps
  |> String.to_seq
  |> Seq.map (function 'L' -> `Left | 'R' -> `Right | _ -> failwith "bad format")
  |> Seq.cycle
  |> map_with_previous (fun prevs x ->
      let a = prevs |> List.map (fun (label, (left, right)) ->
          (* let () = Printf.printf ">%s\n" (DessertMap.to_string (label, (left, right)))  in *)
          match x with
          | `Left -> (left, (List.assoc left dessert_map))
          | `Right -> (right, (List.assoc right dessert_map))
        )
      in
      a, a
    ) (ghost_map_start)
  |> Seq.map (snd)
  (* |> Seq.map (fun xs -> *)
  (*     print_endline "---\n"; *)
  (*     xs *)
  (*     |> List.map (fun x-> let () = Printf.printf "%s\n" (DessertMap.to_string x)  in x )) *)
  (* |> Seq.take 10 *)
  |> Seq.take_while (fun xs ->
      xs
      |> List.exists (fun x -> not @@ List.mem x ghost_map_end))
  |> Seq.length
  |> succ

let () =
  (* let lines = In_channel.input_lines (In_channel.open_text  "day8.txt") in *)
  let lines = In_channel.input_lines (In_channel.open_text "day8part2.example.txt") in
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main_part_2 lines)

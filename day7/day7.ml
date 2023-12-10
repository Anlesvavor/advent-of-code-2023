
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

let destructive_parse_str_num_list (str : string) : int list =
  str
  |> String.split_on_char ' '
  |> List.filter_map (int_of_string_opt)

let rec combinations k list =
  if k <= 0 then [[]]
  else match list with
    | [] -> []
    | h :: tl ->
      let with_h = List.map (fun l -> h :: l) (combinations (k - 1) tl) in
      let without_h = combinations k tl in
      with_h @ without_h

let pset (n : int) (list : 'a list) : ('a list list) =
  let rec aux (n : int) (list' : 'a list list) : 'a list list =
    if n > 1
    then
      aux
        (pred n)
        (List.fold_left (fun acc x -> acc @ (List.map (fun y -> y :: x) list)) [] list')
    else list'
  in
  aux n (list |> List.map (fun x -> [x]))

module Hand = struct
  type handtype = High | OnePair | TwoPair | ThreeOf | Full | FourOf | FiveOf
  type t =
    { cards: int list
    ; bid : int
    ; handt : handtype
    }

  let labels = ['2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A']

  let to_string (t : t) : string =
    let cards =
      List.fold_left (fun acc c -> (String.make 1 (List.nth labels c)) ^ acc) "" t.cards
    in
    let handt =
      match t.handt with
      | FiveOf -> "Five"
      | FourOf -> "Four"
      | Full -> "Full"
      | ThreeOf -> "Three"
      | TwoPair -> "Two Pair"
      | OnePair -> "One Pair"
      | High -> "High"
    in
    Printf.sprintf "%s %s %s" cards (string_of_int t.bid) handt

  let from_string (str : string) : t =
    let read_cards str =
      str
      |> String.fold_left (fun acc c ->
          List.find_index ((=)c) labels
          |> function
          | Some a -> a :: acc
          | None -> failwith @@ Printf.sprintf "Bad format: %s" str
        ) []
    in
    let cards, bid =
      match String.split_on_char ' ' str with
      | cards_str :: bid_str :: [] -> read_cards cards_str, int_of_string bid_str
      | _ -> failwith @@ Printf.sprintf "Bad format: %s" str
    in
    let handt : handtype =
      cards
      |> List.sort compare
      |> group_items (fun a b ->a=b)
      |> List.sort (fun a b -> compare (List.length b) (List.length a))
      (* |> List.map (fun x -> *)
      (*     let () = Printf.printf " "  in *)
      (*     x |> List.map (fun y -> *)
      (*         let () = Printf.printf "%d;" (y) in *)
      (*         y *)
      (*       ) *)
      (*   ) *)
      (* |> (fun x -> let () = print_newline () in x) *)
      |> function
      | [a] when (List.length a) = 5 -> FiveOf
      | a :: b :: _ when (List.length a) = 4 -> FourOf
      | a :: b :: _ when ((List.length a) = 3) && ((List.length b) = 2) -> Full
      | a :: b :: _ when ((List.length a) = 3) -> ThreeOf
      | a :: b :: c :: _ when ((List.length a) = 2) && ((List.length b) = 2) -> TwoPair
      | a :: b :: _ when ((List.length a) = 2) -> OnePair
      | _ -> High
    in
    { cards; bid; handt }

  let compare (a : t) (b : t) : int =
    (* let () = print_endline @@ to_string a in *)
    let handt_result = compare a.handt b.handt in
    if handt_result <> 0
    then handt_result
    else
      List.fold_right2 (fun a b acc ->
          if acc <> 0
          then acc
          else compare a b
        ) a.cards b.cards 0

end

let main lines =
  lines
  |> List.map (Hand.from_string)
  |> List.sort (Hand.compare)
  |> List.map (fun x ->
      let () = Printf.printf "%s \n" (Hand.to_string x) in
      x
    )
  |> List.mapi (fun i (x : Hand.t) -> (succ i) * x.bid)
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day7.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day7.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

(* Part two *)

(* Tried to reutilize this with functors, but Im not good enough yet *)
module Hand2 = struct
  type handtype = High | OnePair | TwoPair | ThreeOf | Full | FourOf | FiveOf
  type t =
    { cards: int list
    ; bid : int
    ; handt : handtype
    }

  let labels = ['J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A']

  let to_string (t : t) : string =
    let cards =
      List.fold_left (fun acc c -> (String.make 1 (List.nth labels c)) ^ acc) "" t.cards
    in
    let handt =
      match t.handt with
      | FiveOf -> "Five"
      | FourOf -> "Four"
      | Full -> "Full"
      | ThreeOf -> "Three"
      | TwoPair -> "Two Pair"
      | OnePair -> "One Pair"
      | High -> "High"
    in
    Printf.sprintf "%s %s %s" cards (string_of_int t.bid) handt

  let from_string (str : string) : t =
    let read_cards str =
      str
      |> String.fold_left (fun acc c ->
          List.find_index ((=)c) labels
          |> function
          | Some a -> a :: acc
          | None -> failwith @@ Printf.sprintf "Bad format: %s" str
        ) []
    in
    let cards, bid =
      match String.split_on_char ' ' str with
      | cards_str :: bid_str :: [] -> read_cards cards_str, int_of_string bid_str
      | _ -> failwith @@ Printf.sprintf "Bad format: %s" str
    in
    let compute_handt cards : handtype =
      cards
      |> List.sort compare
      |> group_items (fun a b -> a = b)
      |> List.sort (fun a b -> compare (List.length b) (List.length a))
      |> function
      | [a] when (List.length a) = 5 -> FiveOf
      | a :: b :: _ when (List.length a) = 4 -> FourOf
      | a :: b :: _ when ((List.length a) = 3) && ((List.length b) = 2) -> Full
      | a :: b :: _ when ((List.length a) = 3) -> ThreeOf
      | a :: b :: c :: _ when ((List.length a) = 2) && ((List.length b) = 2) -> TwoPair
      | a :: b :: _ when ((List.length a) = 2) -> OnePair
      | _ -> High
    in
    let handt =
      if List.exists ((<>) 0) cards
      then compute_handt cards
      else
        let jokers, cards =
          cards
          |> List.partition ((=) 0)
        in
        cards
        |> List.sort_uniq (compare)
        |> pset (List.length jokers)
        |> List.map (List.append cards)
        |> List.map (compute_handt)
        |> List.fold_left (fun acc h ->
            compare acc h
            |> function
            | 0 | -1 -> h
            | 1 -> acc
          ) High
    in
    { cards; bid; handt }

  let compare (a : t) (b : t) : int =
    (* let () = print_endline @@ to_string a in *)
    let handt_result = compare a.handt b.handt in
    if handt_result <> 0
    then handt_result
    else
      List.fold_right2 (fun a b acc ->
          if acc <> 0
          then acc
          else compare a b
        ) a.cards b.cards 0

end

let main_part_2 lines =
  lines
  |> List.map (Hand2.from_string)
  |> List.sort (Hand2.compare)
  |> List.map (fun x ->
      let () = Printf.printf "%s \n" (Hand2.to_string x) in
      x
    )
  |> List.mapi (fun i (x : Hand2.t) -> (succ i) * x.bid)
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day7.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day7.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main_part_2 lines)

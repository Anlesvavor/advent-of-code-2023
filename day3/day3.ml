
let char_is_int (c : char) : bool =
  let code = Char.code c in
  (48 <= code) && (code <= 57)

let () = assert(char_is_int '0')
let () = assert(char_is_int '1')
let () = assert(char_is_int '8')
let () = assert(char_is_int '9')
let () = assert(not( char_is_int 'a' ))

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

let string_to_list (str : string) : char list =
  str
  |> String.fold_left (fun acc x -> x :: acc) []
  |> List.rev

let () = assert(['w';'e'] = string_to_list "we")

let join_chars (cs : char list) : string =
  cs |> List.to_seq |> String.of_seq

let () = assert("abc" = (join_chars ['a'; 'b'; 'c']))

let () =
  let f a b =
    match a, b with
    | a, b when (char_is_int a) && (char_is_int b) -> true
    | _ -> false
  in
  let test = group_items f (string_to_list "...12.#.") in
  (* let () = List.iter (fun cs -> *)
  (*     let () = List.iter (print_char) cs in *)
  (*     print_newline () *)
  (*   ) test *)
  (* in *)
  assert([['.'];['.'];['.'];['1';'2'];['.'];['#'];['.']] = test)

let iota (n : int) : int list =
  let rec iota' n acc =
    if n = 0
    then 0 :: acc
    else iota' (pred n) (n :: acc)
  in
  iota' n []

let () = assert([0;1] = iota 1)

let overall_positions (l : 'a list list) : (('a list) * int) list =
  let rec aux acc pos xs =
    match xs with
    | [] -> acc
    | hd :: tl -> aux ((hd, pos) :: acc) (pos + List.length hd) tl
  in
  aux [] 0 l
  |> List.rev
;;

let () = assert( [([1], 0); ([1;2], 1); ([3], 3)] = (overall_positions [[1]; [1; 2]; [3]]))

module EngineItem = struct
  type t =
    { part_number: int option
    ; symbol: char option
    ; coord: int * int
    ; span: int
    }

  let parse_line (line : string) : (string * int * int) list =
    let predicate a b =
      match a, b with
      | a, b when (char_is_int a) && (char_is_int b) -> true
      | _ -> false
    in
    line
    |> string_to_list
    |> group_items predicate
    |> overall_positions
    |> List.map (fun (cs, pos) ->
        let len = List.length cs in
        (join_chars cs, pos, len)
      )
    |> List.filter (fun (s, _, _) -> not(s = "."))

  let parse_schematic (lines : string list) : t list =
    lines
    |> List.mapi (fun row_num line ->
        line
        |> parse_line
        |> List.map (fun (s, col_num, span) ->
            let coord = (row_num, col_num) in
            let (part_number, symbol) = match int_of_string_opt s with
              | None -> (None, Some (String.get s 0))
              | Some n -> (Some n, None)
            in
            { part_number; symbol; coord; span = (String.length s) }
          )
      )
    |> List.flatten

  let eq (a : t) (b : t) : bool =
    a.part_number = b.part_number
    && a.coord = b.coord
    && a.symbol = b.symbol
    && a.span = b.span

  let to_string (t : t) : string =
    let x, y = t.coord in
    Printf.sprintf "%d %c (%d,%d) %d"
      (Option.value t.part_number ~default:0)
      (Option.value t.symbol ~default:' ')
      x y
      t.span

end

module DiscreteArea = struct
  type 'a t =
    { tag: 'a
    ; area: (int * int) list
    }

  let box_of (coord : int * int) (a : 'a) : 'a t =
    let (x, y) = coord in
    let area =
      [-1; 0 ; 1]
      |> List.map (fun dx ->
          [-1; 0 ; 1]
          |> List.map (fun dy -> (x + dx, y + dy))
        )
      |> List.flatten
    in
    { tag = a; area }

  let contains (target : (int * int)) (box : 'a t) : bool =
    let { area; _ } = box in
    List.mem target area

  let to_string (t : 'a t) : string =
    t.area |> List.fold_left (fun acc (a,b) -> acc^Printf.sprintf "(%d,%d)" a b) ""

  let () = print_endline @@ to_string (box_of (1,1) 0);;

end

let main lines =
  let engine_items =
    lines
    |> EngineItem.parse_schematic
  in
  let () = (engine_items |> List.iter (fun x -> print_endline @@ (EngineItem.to_string x))) in
  let (symbols, part_numbers) =
    engine_items
    |> List.partition (fun (item : EngineItem.t) ->
        match item.part_number with
        | None -> true
        | _ -> false
      )
  in
  let associated_sym_with_pn =
    let boxed_symbols =
      symbols
      |> List.map (fun (sym : EngineItem.t) ->
          DiscreteArea.box_of sym.coord sym
        )
    in
    let part_numbers_at_coord =
      part_numbers
      |> List.map (fun (pn : EngineItem.t) ->
          let (x, y) = pn.coord in
          pn.span
          |> pred
          |> iota
          |> List.map (fun i -> ((x , y + i), pn))
        )
      |> List.flatten
    in
    boxed_symbols
    |> List.filter_map (fun boxed_symbol ->
        part_numbers_at_coord
        |> List.find_all( fun ((x, y), (pn : EngineItem.t)) ->
            DiscreteArea.contains (x, y) boxed_symbol
          )
        |> List.sort_uniq (fun (_, (a : EngineItem.t)) (_, (b : EngineItem.t)) ->
            if EngineItem.eq a b then 0 else 1
          )
        |> function
        | [] -> None
        | a -> Some (a |> List.map (fun (_,pn) -> (boxed_symbol, pn)))
      )
    |> List.flatten
  in
  let () =
    associated_sym_with_pn
    |> List.iter (fun ((a : EngineItem.t DiscreteArea.t), (b : EngineItem.t)) ->
        let cc = (a.tag : EngineItem.t) in
        let (x, y) = cc.coord in
        let (c : char) =
          cc
          |> (fun x -> x.symbol)
          |> function
          | None -> failwith "This doesnt happend; to jaded to code it as unrepresentable"
          | Some a -> a
        in
        Printf.printf "%c[%d,%d] %d\n" c x y (match b.part_number with | None -> -1 | Some a -> a)
      )
  in
  associated_sym_with_pn
  |> List.fold_left (fun acc (_, (pn : EngineItem.t)) ->
      match pn.part_number with
      | None -> failwith "This doesnt happend; to jaded to code it as unrepresentable"
      | Some a -> a + acc
    ) 0


let () =
  let lines = In_channel.input_lines (In_channel.open_text  "3.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "3.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

(* part two *)

let main_part_two lines =
  let engine_items =
    lines
    |> EngineItem.parse_schematic
  in
  let () = (engine_items |> List.iter (fun x -> print_endline @@ (EngineItem.to_string x))) in
  let (symbols, part_numbers) =
    engine_items
    |> List.partition (fun (item : EngineItem.t) ->
        match item.part_number with
        | None -> true
        | _ -> false
      )
  in
  let associated_sym_with_pn =
    let boxed_symbols =
      symbols
      |> List.map (fun (sym : EngineItem.t) ->
          DiscreteArea.box_of sym.coord sym
        )
    in
    let part_numbers_at_coord =
      part_numbers
      |> List.map (fun (pn : EngineItem.t) ->
          let (x, y) = pn.coord in
          pn.span
          |> pred
          |> iota
          |> List.map (fun i -> ((x , y + i), pn))
        )
      |> List.flatten
    in
    boxed_symbols
    |> List.filter_map (fun boxed_symbol ->
        let (pns : EngineItem.t list) =
          part_numbers_at_coord
          |> List.find_all( fun ((x, y), (pn : EngineItem.t)) ->
              DiscreteArea.contains (x, y) boxed_symbol
            )
          |> List.sort_uniq (fun (_, (a : EngineItem.t)) (_, (b : EngineItem.t)) ->
              if EngineItem.eq a b then 0 else 1
            )
          |> List.map (fun (a, b) -> b)
        in
        match pns with
        | [] -> None
        | a -> Some (boxed_symbol, pns)
      )
  in
  associated_sym_with_pn
  |> List.fold_left (fun acc ((item : EngineItem.t DiscreteArea.t), (pns : EngineItem.t list)) ->
      let (piece : EngineItem.t) = item.tag in
      acc + match piece.symbol, (List.length pns) with
      | Some '*', 2 ->
        pns
        |> List.fold_left (fun acc' (pn : EngineItem.t) ->
            match pn.part_number with
            | None -> failwith "This doesnt happens; to jaded to code it as unrepresentable"
            | Some a -> a * acc'
          ) 1
      | _ -> 0
    ) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "3.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "3.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main_part_two lines)

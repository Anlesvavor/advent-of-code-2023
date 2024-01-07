let bounded lower upper x =
  let lower, upper = if lower > upper then upper, lower else lower, upper in
  if lower > x
  then None
  else if x > upper
  then None
  else Some x

let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let cons_opt a list =
  match a with
  | Some a -> a :: list
  | None -> list

let uniq list =
  list
  |> List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) []

let string_of_list list =
  list |> List.to_seq |> String.of_seq

module Laser = struct
  type orientation = North | East | South | West
  type split = Split of orientation * orientation | Pass of orientation
  type tile = Vertical | Horizontal | Forward | Backward | None
  type elt = (int * int) * tile
  type t = elt list

  let from_char (row : int) (col : int) (c : char) : elt =
    let mirror = match c with
      | '|' -> Vertical
      | '-' -> Horizontal
      | '/' -> Forward
      | '\\' -> Backward
      | '.' -> None
      | _ -> failwith "Bad data!"
    in
    (row, col), mirror

  let from_string (row : int) (str : string) : t =
    str |> String.to_seq |> Seq.mapi (fun i c -> from_char  row i c) |> List.of_seq

  let from_lines (lines : string list) : t =
    lines |> List.mapi (from_string) |> List.flatten

  let condense powered =
    let powered = List.map (fun ((row, col), _) -> (row, col)) powered in
    uniq powered

  let print_powered powered =
    let powered =
      powered
      |> List.map (fun ((row, col), _) -> (row, col))
      |> uniq
    in
    let max_row = List.fold_left (fun acc (r,_) -> max acc r) 0 powered in
    let max_col = List.fold_left (fun acc (_,c) -> max acc c) 0 powered in
    let board = List.init (succ max_row) (fun row ->
        List.init (succ max_col) (fun col ->
            if List.mem (row, col) powered
            then '#'
            else '.'
          )
      )
    in
    board
    |> List.iter (fun row ->
        print_endline @@ string_of_list row
      )

  let cast (row : int) (col : int) (orientation : orientation) (t : t) =
    let max_row = List.fold_left (fun acc ((r, _) ,_) -> max acc r) 0 t in
    let max_col = List.fold_left (fun acc ((_, c) ,_) -> max acc c) 0 t in
    let step row col orientation =
      match orientation with
      | East ->
        let* col' = bounded 0 max_col (succ col) in
        Some (row, col')
      | South ->
        let* row' = bounded 0 max_row (succ row) in
        Some (row', col)
      | West ->
        let* col' = bounded 0 max_col (pred col) in
        Some (row, col')
      | North ->
        let* row' = bounded 0 max_row (pred row) in
        Some (row', col)
    in
    let get_next_orientation cur_orientation next_tile =
      (* Perspective from the current tile *)
      match cur_orientation, next_tile with
      | East, Vertical | West, Vertical -> Split (North, South)
      | North, Vertical -> Pass North
      | South, Vertical -> Pass South
      | North, Horizontal | South, Horizontal -> Split (East, West)
      | East, Horizontal -> Pass East
      | West, Horizontal -> Pass West
      | North, Backward -> Pass West
      | East, Backward -> Pass South
      | South, Backward -> Pass East
      | West, Backward -> Pass North
      | North, Forward -> Pass East
      | East, Forward -> Pass North
      | South, Forward -> Pass West
      | West, Forward -> Pass South
      | a, None -> Pass a
    in
    let get_tile row col =
      let* row' = bounded 0 max_row row in
      let* col' = bounded 0 max_col col in
      let* tile = List.assoc_opt (row', col') t in
      Some tile
    in
    let rec walk acc_powered (stack : ((int * int) * orientation * tile) list) =
      match stack with
      | [] -> acc_powered
      | ((row, col) as cur_coord, cur_orientation, cur_tile) :: rest ->
        begin
          if (List.mem (cur_coord, cur_orientation) acc_powered) && (not (cur_tile = Backward || cur_tile = Forward))
          then walk (acc_powered) rest
          else
            let get_next_stack_item ornt =
              let* (next_row, next_col) as next_coord = step row col ornt in
              let* next_tile = get_tile next_row next_col in
              Some (next_coord, ornt, next_tile)
            in
            match get_next_orientation cur_orientation cur_tile with
            | Pass a ->
              let stack' = cons_opt (get_next_stack_item a) rest in
              walk ((cur_coord, cur_orientation) :: acc_powered) stack'
            | Split (a, b) ->
              let stack' =
                rest
                |> cons_opt (get_next_stack_item a)
                |> cons_opt (get_next_stack_item b)
              in
              walk ((cur_coord, cur_orientation) :: acc_powered) stack'
        end
    in
    let initial_tile = List.assoc (row,col) t in
    walk [] [(row,col), orientation, initial_tile]
    |> condense

end

let main lines =
  let board = Laser.from_lines lines in
  board
  |> Laser.cast 0 0 Laser.East
  |> List.length

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day16.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day16.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main lines)


let main_part_2 lines =
  let board = Laser.from_lines lines in
  let max_row = List.fold_left (fun acc ((r, _) ,_) -> max acc r) 0 board in
  let max_col = List.fold_left (fun acc ((_, c) ,_) -> max acc c) 0 board in
  let north_border = List.init (succ max_col) (fun col -> 0, col, Laser.South) in
  let south_border = List.init (succ max_col) (fun col -> max_row, col, Laser.North) in
  let west_border = List.init (succ max_row) (fun row -> row, 0, Laser.East) in
  let east_border = List.init (succ max_row) (fun row -> row, max_col, Laser.West) in
  let work border =
    List.fold_left (fun acc (row, col, orientation) ->
        let len =
          Laser.cast row col orientation board
          |> List.length
        in
        if len > acc then len else acc
      ) 0 border
  in
  let d1 = Domain.spawn (fun _ -> work north_border) in
  let d2 = Domain.spawn (fun _ -> work south_border) in
  let d3 = Domain.spawn (fun _ -> work west_border) in
  let d4 = Domain.spawn (fun _ -> work east_border) in
  [Domain.join d1; Domain.join d2; Domain.join d3; Domain.join d4] |>
  List.fold_left (fun acc x ->
      max (x) acc
    ) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day16.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day16.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_int (main_part_2 lines)

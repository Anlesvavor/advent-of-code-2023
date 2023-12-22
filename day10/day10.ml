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

module Pipeline = struct
  type state = Cycles | Dies
  type orientation = North | East | South | West
  type endpoint = (orientation * orientation)
  type piece = Pipe of endpoint | Ground | Start
  type elt = ((int * int) * piece)
  type t = elt list

  let from_char (row : int) (col : int) (c : char) : elt =
    let pipe = match c with
      | '|' -> Pipe (North, South)
      | '-' -> Pipe (East, West)
      | 'L' -> Pipe (North, East)
      | 'J' -> Pipe (North, West)
      | '7' -> Pipe (South, West)
      | 'F' -> Pipe (South, East)
      | '.' -> Ground
      | 'S' -> Start
      | _ -> failwith "Bad data!"
    in
    (row, col), pipe

  let to_string (elt : elt) : string =
    let ((x, y), _) = elt in
    let piece = match snd elt with
      | Pipe (North, South) | Pipe (South, North) -> '|'
      | Pipe (East, West) | Pipe (West, East) -> '-'
      | Pipe (North, East) | Pipe (East, North) -> 'L'
      | Pipe (North, West) | Pipe (West, North) -> 'J'
      | Pipe (South, West) | Pipe (West, South) -> '7'
      | Pipe (South, East) | Pipe (East, South) -> 'F'
      | Ground -> '.'
      | Start -> 'S'
      | _ -> failwith "Bad format"
    in
    Printf.sprintf "(%d,%d) %c" x y piece

  let to_strings (t : t) : string =
    t |> List.fold_left (fun acc s -> acc ^ (Printf.sprintf "%s; " @@ to_string s)) ""

  let from_string (row : int) (str : string) : elt list =
    str |> list_of_string |> List.mapi (fun i c -> from_char i row c)

  let from_lines (lines : string list) : t =
    lines |> List.mapi (from_string) |> List.flatten

  let get_start (t : t) : elt =
    t |> List.find (fun (_, p) -> p = Start)

  let neighboors (anchor : elt) (pipemap : t) : (elt * orientation) list =
    let find coord = pipemap |> List.find_opt (fun (coord', _) -> coord' = coord) in
    let ((x, y), piece) = anchor in
    let top = (x, y - 1) |> find in
    let right = (x + 1, y) |> find in
    let bottom = (x, y + 1) |> find in
    let left = (x - 1, y) |> find in
    [top, North; right, East; bottom, South; left, West]
    |> List.filter_map (fun (a, b) ->
        match a with
        | Some a -> Some (a, b)
        | None -> None
      )

  let reflection =
    function
    | North -> South
    | East -> West
    | South -> North
    | West -> East

  let construct (anchor : elt) (pipemap : t) =
    let can_connect_on (a_orientation : orientation) (b : elt) : ((orientation * orientation) option) =
      let _, bpiece = b in
      match bpiece with
      | Ground -> None
      | Start -> Some (a_orientation, a_orientation)
      | Pipe (bor1, bor2) ->
        if (reflection bor1) = a_orientation
        then Some (reflection bor1, bor2)
        else if (reflection bor2) = a_orientation
        then Some (reflection bor2, bor1)
        else None
    in
    let rec chain acc (cur : orientation * elt)  =
      let (next_orientation, anchor) = cur in
      let neighboors = neighboors anchor pipemap in
      let next_anchor =
        let candidate_anchor =
          neighboors |> List.find_map (fun (x, ornt) ->
              if next_orientation = ornt
              then Some x
              else None
            )
        in
        candidate_anchor
        |*> (can_connect_on next_orientation)
        |*> (fun x ->
            match candidate_anchor with
            | Some a -> Some (a, x)
            | None -> None
          )
      in
      match next_anchor with
      | Some (anchor, (_matched_ornt, next_ornt)) -> chain (anchor::acc) (next_ornt, anchor)
      | None -> acc |> List.rev
    in
    neighboors anchor pipemap
    |> List.filter_map (fun (x, ornt) ->
        match can_connect_on ornt x with
        | Some (_matched_ornt, next_ornt) -> Some(next_ornt, x)
        | None -> None
      )
    |> List.map (fun x -> chain [] x)

end

let main lines =
  let pipemap =
    lines
    |> Pipeline.from_lines
  in
  let starting_pipe =
    pipemap
    |> Pipeline.get_start
  in
  let pipelines = Pipeline.construct starting_pipe pipemap in
  pipelines
  |> also (List.iter (fun x -> x |> Pipeline.to_strings |> print_string))
  |> List.hd
  |> List.length
  |> (fun x -> int_of_float @@ ceil (div' x 2))


let () =
  let lines = In_channel.input_lines (In_channel.open_text  "day10.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "day10.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)
(* main lines |> List.iter (print_endline) *)

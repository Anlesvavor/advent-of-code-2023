let list_of_string str =
  str |> String.to_seq |> List.of_seq

let take_if (predicate : 'a -> bool) (a : 'a) : 'a option=
  if predicate a then Some a else None


let take_if_opt (predicate : 'a -> bool) (a : 'a option) : 'a option=
  Option.bind a (fun a -> if predicate a then Some a else None)

module Pipeline = struct
  type state = Cycles | Dies
  type pipe = Ns | Ew | Ne | Nw | Sw | Se | G | S
  type orientation = North | East | South | West | Start
  type elt = ((int * int) * pipe)
  type t = elt list

  let from_char (row : int) (col : int) (c : char) : elt =
    let pipe = match c with
      | '|' -> Ns
      | '-' -> Ew
      | 'L' -> Ne
      | 'J' -> Nw
      | '7' -> Sw
      | 'F' -> Se
      | '.' -> G
      | 'S' -> S
      | _ -> failwith "Bad data!"
    in
    (row, col), pipe

  let to_string (elt : elt) : string =
    let ((x, y), pipe) = elt in
    let pipe = match snd elt with
      | Ns -> '|'
      | Ew -> '-'
      | Ne -> 'L'
      | Nw -> 'J'
      | Sw -> '7'
      | Se -> 'F'
      | G -> '.'
      | S -> 'S'
    in
    Printf.sprintf "(%d,%d) %c" x y pipe

  let to_strings (t : t) : string =
    t |> List.fold_left (fun acc s -> acc ^ (Printf.sprintf "%s; " @@ to_string s)) ""

  let from_string (row : int) (str : string) : elt list =
    str |> list_of_string |> List.mapi (fun i c -> from_char i row c)

  let from_lines (lines : string list) : t =
    lines |> List.mapi (from_string) |> List.flatten

  let get_start (t : t) : elt =
    t |> List.find (fun (_, p) -> p = S)

  let neighboors (anchor : elt) (pipemap : t) : (elt option * elt option * elt option * elt option) =
    let find coord = pipemap |> List.find_opt (fun (coord', _) -> coord' = coord) in
    let ((x, y), pipe) = anchor in
    let top = (x, y - 1) |> find in
    let right = (x + 1, y) |> find in
    let bottom = (x, y + 1) |> find in
    let left = (x - 1, y) |> find in
    (top, right, bottom, left)

  let can_be (valid_pipes : pipe list) (elt : elt) : bool =
    let (_, pipe) = elt in
    List.mem pipe valid_pipes

  let can_be_north = can_be [Ns; Sw; Se]
  let can_be_east = can_be [Ew; Nw; Sw]
  let can_be_south = can_be [Ns; Nw; Ne]
  let can_be_west = can_be [Ew; Ne; Se]

  let can_connect (source : elt) (destination : elt) : bool =
    let (_, source_pipe) = source in
    let (_, destination_pipe) = destination in
    match destination_pipe with
    | Ns -> (match source_pipe with Ew -> false | _ -> true)
    | Ew -> (match source_pipe with Ns -> false | _ -> true)
    | Ne -> (match source_pipe with Ne -> false | _ -> true)
    | Nw -> (match source_pipe with Nw -> false | _ -> true)
    | Se -> (match source_pipe with Se -> false | _ -> true)
    | Sw -> (match source_pipe with Sw -> false | _ -> true)
    | G -> false
    | S -> true

  let construct (anchor : elt) (pipemap : t) =
    let aux
        (coming_from : orientation)
        (anchor : elt)
      : ((elt * orientation) list, elt * orientation) Either.t
      =
      let criteria (dir : elt option) f g new_or =
        dir
        |> take_if_opt (f)
        |> take_if_opt (g)
        |> take_if_opt (can_connect anchor)
        |> Option.map (fun x -> (x, new_or))
      in
      let (top, right, bottom, left) = neighboors anchor pipemap in
      let top' = criteria top can_be_north (fun _ -> coming_from <> North) South in
      let right' = criteria right can_be_east (fun _ -> coming_from <> East) West in
      let bottom' = criteria bottom can_be_south (fun _ -> coming_from <> South) North in
      let left' = criteria left (can_be_west) (fun _ -> coming_from <> West) East in
      let valid = [top'; right'; bottom'; left'] in
      let ground' = ((-1, -1), G), North in
      match coming_from with
      | Start -> valid |> List.filter_map (fun x -> x) |> Either.left
      | _ -> valid |> List.find_opt (Option.is_some) |> Option.join |> Option.value ~default:ground' |> Either.right
    in
    let rec recc acc coming_from hd =
      (* function *)
      (* | [] -> acc *)
      (* | hd :: tl -> begin *)
      let ((_, pipe) as result, coming_from') = match aux coming_from hd with
        | Right a -> a
        | _ -> failwith "Shouldnt happen"
      in
      match pipe with
      | G | S -> acc
      | _ -> recc (result :: acc) coming_from' result
      (* end *)
    in
    aux Start anchor
    |> function
    | Left a ->
      a |> List.map (fun (anchor', orientation) ->
          recc [] orientation anchor'
        )
    | _ -> failwith "Shouldnt happen; This is the first iteration"

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
  (* |> List.map (Pipeline.to_strings) *)
  pipelines
  |> List.map (fun x ->
      x
      |>
      Pipeline.to_strings
    )


let () =
  (* let lines = In_channel.input_lines (In_channel.open_text  "day10.txt") in *)
  let lines = In_channel.input_lines (In_channel.open_text "day10.example.txt") in
  let () = List.iter (Printf.printf "%s\n") lines in
  (* print_string (main lines) *)
  main lines |> List.iter (print_endline)

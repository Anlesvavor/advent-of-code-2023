
let int_of_string' (context : string) (a : string) : int =
  match int_of_string_opt a with
  | Some a -> a
  | None -> failwith @@ Printf.sprintf "Malformed input: '%s'" context

module Game = struct
  type t =
    { id : int
    ; cubes : (string * int) list
    }

  let cubes_to_string (cubes : (string * int) list) : string =
    List.fold_left (fun acc (c, a) -> acc ^ (Printf.sprintf "%d %s, " a c)) "" cubes

  let to_string (game : t) : string =
    let {id; cubes} = game in
    Printf.sprintf "id: %d| cubes: " id
    ^ cubes_to_string cubes

  let from_string (colors : string list) (str : string) : t =
    let game_str, cubes_str = match String.split_on_char ':' str with
      | game_str :: cubes_str :: [] -> (game_str, cubes_str)
      | _ -> failwith @@ Printf.sprintf "Malformed input: %s" str
    in
    let game_id =
      String.split_on_char ' ' game_str
      |> List.rev
      |> List.hd
      |> int_of_string' (str^" at 22")
    in
    let cubes =
      let sets =
        cubes_str
        |> String.split_on_char ';'
        |> List.map String.trim
      in
      let cubes_from_set (set : string) : (string * int) list =
        set
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.map (fun l ->
            match String.split_on_char ' ' l with
            | count :: color :: [] -> (color, int_of_string' (count^" - "^color^" at 36") count)
            | _ -> failwith @@ Printf.sprintf "Malformed input: %s" set
          )
      in
      sets
      (* |> List.map (fun x -> let y = cubes_from_set x in let () = Printf.printf "->%s" @@ cubes_to_string y in y) *)
      |> List.map cubes_from_set
      |> List.fold_left (fun acc (set : (string * int) list) ->
          if List.length acc = 0
          then set
          else
            acc
            |> List.map (fun (acc_color, acc_count) ->
                let set_count = match List.assoc_opt acc_color set with
                  | None ->
                    begin
                      (* let () = Printf.printf "&&%s\n" acc_color in *)
                      (* let () = Printf.printf "!!%s\n" @@ cubes_to_string acc in *)
                      (* let () = Printf.printf "##%s\n" @@ cubes_to_string set in *)
                      0
                    end
                  | Some a -> a
                in
                (acc_color, max acc_count set_count)
              )
        ) (List.map (fun i -> (i,0)) colors)
    in
    (* let () = Printf.printf "%s\n" @@ cubes_to_string cubes in *)
    { id = game_id; cubes = cubes }

  let is_valid_game (real_cubes: (string * int) list) (game : t) : bool =
    let { cubes; _ } = game in
    cubes
    |> List.fold_left (fun acc (color, amount) ->
        acc && match List.assoc_opt color real_cubes with
        | None -> true
        | Some real_cube_amount -> real_cube_amount >= amount
      ) true

end

let main lines =
  let real_cubes = ["red", 12; "green", 13; "blue", 14] in
  lines
  |> List.map (fun x ->
      (* let () = Printf.printf "%s\n" x in *)
      let y = Game.from_string ["red"; "green"; "blue"] x in
      let () = Printf.printf "%s\n" @@ Game.to_string y in
      let () = Printf.printf "\n" in
      y
    )
  |> List.find_all (Game.is_valid_game real_cubes)
  |> List.fold_left (fun acc ({id; _}: Game.t) -> acc + id) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "2.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "2.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

(* Part two *)

let main lines =
  lines
  |> List.map (fun x ->
      let y = Game.from_string ["red"; "green"; "blue"] x in
      let () = Printf.printf "%s\n" @@ Game.to_string y in
      let () = Printf.printf "\n" in
      y
    )
  |> List.map (fun ({cubes; _}: Game.t) ->
      List.fold_left (fun acc (_, amount) -> acc * amount) 1 cubes
    )
  |> List.fold_left (+) 0

let () =
  let lines = In_channel.input_lines (In_channel.open_text  "2.txt") in
  (* let lines = In_channel.input_lines (In_channel.open_text "2.example.txt") in *)
  let () = List.iter (Printf.printf "%s\n") lines in
  print_int (main lines)

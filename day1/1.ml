let example_file = "1.example.txt"
let input = "1.txt"

(* Because Ocaml std doesn't have it :/ *)
let string_rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))

let char_is_int (c : char) : bool =
  let code = Char.code c in
  (48 <= code) && (code <= 57)

let () = assert(char_is_int '0')
let () = assert(char_is_int '1')
let () = assert(char_is_int '8')
let () = assert(char_is_int '9')
let () = assert(not( char_is_int 'a' ))

let first_num_in_string (str : string) : int =
  let num =
    str
    |> String.to_seq
    |> Seq.find char_is_int
  in
  match num with
  | None -> failwith "That elve removed a value!"
  | Some a -> (int_of_char a) - (int_of_char '0')

let main (lines : string list)=
  let f acc s =
    let first = first_num_in_string s in
    let last = first_num_in_string (string_rev s) in
    let result = (first * 10) + last in
    (* let () = print_int result in *)
    (* let () = print_newline () in *)
    acc + result
  in
  List.fold_left f 0 lines

let () =
  let lines = In_channel.input_lines (In_channel.open_text input) in
  print_int (main lines)

(* part two *)
let iota (n : int) : int list =
  let rec iota' n acc =
    if n = 0
    then 0 :: acc
    else iota' (pred n) (n :: acc)
  in
  iota' n []

let () = assert([0;1] = iota 1)

let num_dict : (string * int) list =
  [ ("one", 1)
  ; ("two", 2)
  ; ("three", 3)
  ; ("four", 4)
  ; ("five", 5)
  ; ("six", 6)
  ; ("seven", 7)
  ; ("eight", 8)
  ; ("nine", 9)
  ; ("1" , 1)
  ; ("2" , 2)
  ; ("3" , 3)
  ; ("4" , 4)
  ; ("5" , 5)
  ; ("6" , 6)
  ; ("7" , 7)
  ; ("8" , 8)
  ; ("9" , 9)
  ; ("0" , 0)
  ]

let substring_index (str : string) (substr : string) : int option =
  let substr_len = String.length substr in
  let str_len = String.length str in
  if substr_len > str_len
  then None
  else
    let delta_len = str_len - substr_len in
    iota delta_len
    |> List.find_index (fun (i : int) -> substr = String.sub str i substr_len)

let first_and_last_num_in_string (str : string) : (int * int) =
  let len = String.length str in
  let written_nums_indexes =
    iota (pred len)
    |> List.map (fun i -> (String.sub str i (len - i), i))
    |> List.fold_left (fun acc (s, i) ->
        let x =
          num_dict
          |> List.find_map (fun (s', n) ->
              if String.starts_with ~prefix:s' s
              then Some (n, i)
              else None
            )
        in
        match x with
        | None -> acc
        | Some (n, i) -> n :: acc
      ) []
  in
  let () = List.iter (fun x -> (Printf.printf "%d " x)) written_nums_indexes in
  let () = assert(0 < List.length written_nums_indexes) in
  let last = written_nums_indexes |> List.hd in
  let first = written_nums_indexes |> List.rev |> List.hd in
  (first, last)

let main_part_two (lines : string list)=
  let f acc s =
    let (first, last) = first_and_last_num_in_string s in
    let result = (first * 10) + last in
    let () = Printf.printf "%s %d" s result in
    let () = print_newline () in
    acc + result
  in
  List.fold_left f 0 lines


let () =
  (* let lines = In_channel.input_lines (In_channel.open_text "1part2.example.txt") in *)
  let lines = In_channel.input_lines (In_channel.open_text input) in
  let () = print_newline () in
  print_int (main_part_two lines)

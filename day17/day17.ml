
let inf = (Int.max_int / 2)

let digits_of_string str =
  str
  |> String.to_seq
  |> Seq.map (fun x -> (int_of_char x) - Char.code '0')
  |> List.of_seq

let hd_opt = function
  | [] -> None
  | x :: _ -> Some x

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

let update (f : 'a -> 'b) (value : 'a) (list : 'a list) : 'a list =
  let value' = f value in
  if List.exists (fun x -> (f x) = value') list
  then List.map (fun x -> if (f x) = value' then value else x) list
  else list

let upsert (f : 'a -> 'b) (value : 'a) (list : 'a list) : 'a list =
  if List.exists (fun x -> (f x) = (f value)) list
  then List.map (fun x -> if (f x) = (f value) then value else x) list
  else list @ [value]

let manhattan_distance (a_row, a_col) (b_row, b_col) =
  (abs (b_row - a_row)) + (abs (b_col - a_col))

let mem_proj (proj : 'a -> 'b) (b : 'b) (list : 'a list) : bool =
  list
  |> List.to_seq
  |> Seq.exists (fun a -> (proj a) = b)

type direction = North | East | South | West

let string_of_direction =
  function
  | North -> "N"
  | East -> "E"
  | South -> "S"
  | West -> "W"

let neighboors_of_coord (coord : int * int) (board : (int * int) list) : ((int * int) * direction) list =
  let max_row = List.fold_left (fun acc coord ->
      max acc (fst coord)
    ) 0 board
  in
  let max_col = List.fold_left (fun acc coord ->
      max acc (snd coord)
    ) 0 board
  in
  let (row, col) = coord in
  let e =
    let* col' = bounded 0 max_col (succ col) in
    Some ((row, col'), East)
  in
  let s =
    let* row' = bounded 0 max_row (succ row) in
    Some ((row', col), South)
  in
  let w =
    let* col' = bounded 0 max_col (pred col) in
    Some ((row, col'), West)
  in
  let n =
    let* row' = bounded 0 max_row (pred row) in
    Some ((row', col), North)
  in
  List.fold_right (cons_opt) [n;e;s;w] []

module Node = struct
  type t =
    { coord : int * int
    ; weight : int
    ; route_to_node : (t * direction) option
    }

  let compare a b =
    let (a_row, a_col) = a.coord in
    let (b_row, b_col) = b.coord in
    match compare a_row b_row with
    | 0 -> Stdlib.compare a_col b_col
    | a -> a

  let nodes_from_lines (lines : string list) : t list =
    lines
    |> List.mapi (fun row s ->
        s
        |> digits_of_string
        |> List.mapi (fun col d ->
            { coord = (row, col)
            ; weight = d
            ; route_to_node = None
            }
          )
      )
    |> List.flatten

  let distance (a : t) (b : t) : int =
    manhattan_distance (a.coord) (b.coord)

  let rec traverse_path (t : t) () : (t * direction) Seq.node =
    match t.route_to_node with
    | None -> Seq.Nil
    | Some (next, dir) -> Seq.Cons ((next, dir), traverse_path next)

  let string_of_node (t : t) : string =
    Printf.sprintf "%d(%d,%d)" t.weight (fst t.coord) (snd t.coord)

  let string_of_path (t : t) =
    traverse_path t
    |> Seq.fold_left (fun acc (n, d) ->
        acc ^ (string_of_node n) ^ (string_of_direction d) ^ "; "
      ) ""

  let directions_of_path (t : t) : direction list =
    traverse_path t
    |> Seq.map (snd)
    |> List.of_seq

  let eq_coord (a : t) (b : t) : bool =
    a.coord = b.coord

end

module PQueue = struct
  type 'a t = ('a * int) list

  let empty = ([] : 'a t)

  let enqueue (comp : 'a -> int) (a : 'a) (queue : 'a t) : 'a t =
    let p = comp a in
    let rec aux acc =
      function
      | [] -> acc @ [(a, p)]
      | (hd, hd_p) :: tl ->


        if p < hd_p
        then acc @ ((a, p) :: (hd, hd_p) :: tl)
        else aux (acc @ [(hd, hd_p)]) tl
    in
    aux [] queue

  let queue_of_list (f : 'a -> int) (list : 'a list) : 'a t =
    List.fold_left (fun q x -> enqueue f x q) empty list

  let dequeue (queue : 'a t) : ('a option) * ('a t) =
    match queue with
    | (hd, _) :: tl -> (Some hd), tl
    | [] -> None, []

  let pop_assoc (a : 'a) (queue : 'a t) : ('a option) * ('a t) =
    match List.find_opt (fun (a', _) -> a' = a) queue with
    | Some a -> (Some (fst a), List.filter ((<>) a) queue)
    | None -> (None, queue)

  (* enqueue (snd) ('c',1) |> enqueue (snd) ('e', 20) |> enqueue (snd) ('f', 2) |> enqueue (snd) ('g', 1) *)
  (* [(('c', 1), 1); (('g', 1), 1); (('f', 2), 2); (('a', 10), 10); (('b', 10), 10); *)
  (*  (('e', 20), 20)] *)

end

let auto_fold_opt (f : 'a -> 'a -> 'a) (list : 'a list) : 'a option =
  match list with
  | hd :: tl -> Some (List.fold_left f hd tl)
  | [] ->  None

let auto_fold (f : 'a -> 'a -> 'a) (list : 'a list) : 'a =
  match auto_fold_opt f list with
  | Some a -> a
  | None -> failwith "auto fold failed!!!"

let min_elt f = auto_fold (fun a b -> if (f a) < (f b) then a else b)

module Dijkstra = struct

  let distance (a : Node.t) (b : Node.t) : int =
    abs (b.weight - a.weight)

  let min_distance (node : Node.t) (nodes_with_distance : (Node.t * int) list) : (Node.t * int) =
    match nodes_with_distance with
    | hd :: tl -> List.fold_left (fun acc n ->
        if (snd acc) < (snd n)
        then acc
        else n
      ) hd tl
    | [] -> failwith "no nodes?!"

  let neighboors_of_node (node : Node.t) (nodes : Node.t list) : ((Node.t * direction) list) =
    let nodes_coords = (nodes |> List.map (fun (n : Node.t) -> n.coord)) in
    let neighboors_coords = neighboors_of_coord node.coord nodes_coords in
    List.filter_map (fun (n : Node.t) ->
        let* dir = List.assoc_opt n.coord neighboors_coords in
        Some (n, dir)
      ) nodes

  let shortest_path (starter_node : Node.t)  (nodes : Node.t list) =
    let initial_distances = List.map (fun n ->
        if n = starter_node
        then (n, 0)
        else (n, inf)
      ) nodes
    in
    let queue = PQueue.queue_of_list (fun (n : Node.t) -> n.weight) nodes in
    let initial_visited = [] in
    let rec loop (distances : (Node.t * int) list) =
      function
      | [] -> (distances)
      | (queue : Node.t PQueue.t) ->
        let min_node, dist =
          List.map (fun ((n : Node.t), _) ->
              let (_, d) = List.find (fun (x, _) ->
                  Node.eq_coord x n
                ) distances
              in
              (n, d)
            ) queue
          |> min_elt (snd)
        in
        let (cur_opt, (queue' : Node.t PQueue.t)) = PQueue.pop_assoc min_node queue in
        let cur = Option.get cur_opt in
        let neighboors_of_cur = neighboors_of_node cur nodes in
        let neighboors_of_cur_still_in_q =
          neighboors_of_cur
          |> List.filter (fun ((n : Node.t), _) ->
              mem_proj (fst) n queue'
            )
        in
        let distances' =
          neighboors_of_cur_still_in_q
          |> List.fold_left (fun distance_acc ((n : Node.t), dir) ->
              let get_distance node =
                List.assoc_opt node distances
                |> Option.value ~default:(inf)
                (* |> Option.value ~default:(failwith "No distance found for node") *)
              in
              let alt = n.weight + get_distance cur in
              if alt < get_distance n
              then update (fun (k, v) -> k) (n, alt) distances
              else distance_acc
            ) distances
        in
        loop distances' queue'
    in
    loop initial_distances queue

end


let main_2 lines =
  let nodes = Node.nodes_from_lines lines in
  let starter_node = List.find (fun (n : Node.t) -> n.coord = (0, 0)) nodes in
  Dijkstra.shortest_path starter_node nodes
;;

In_channel.input_lines (In_channel.open_text  "day17.example.txt")
|> main_2
(* let () = *)
(*   (\* let lines = In_channel.input_lines (In_channel.open_text  "day17.txt") in *\) *)
(*   let lines = In_channel.input_lines (In_channel.open_text "day17.example.txt") in *)
(*   let () = List.iter (Printf.printf "%s\n") lines in *)
(*   let () = print_newline () in *)
(*   print_endline (main lines_2) *)
(* (main lines) |> List.iter (print_int) *)
(* print_int @@ main lines *)

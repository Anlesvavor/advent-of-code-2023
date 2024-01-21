
let inf = (Int.max_int / 2)

let digits_of_string str =
  str
  |> String.to_seq
  |> Seq.map (fun x -> (int_of_char x) - Char.code '0')
  |> List.of_seq

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
            }
          )
      )
    |> List.flatten

  let get_weight (t : t) : int =
    t.weight

  let string_of_node (t : t) : string =
    Printf.sprintf "%d(%d,%d)" t.weight (fst t.coord) (snd t.coord)

  let eq_coord (a : t) (b : t) : bool =
    a.coord = b.coord

end

module Heap = struct
  type 'a t = Empty | Node of 'a * 'a t * 'a t * int

  let singleton a = Node (a, Empty, Empty, 1)

  let rank = function
    | Empty -> 0
    | Node (_, _, _, r) -> r

  let rec merge t1 t2 =
    match t1, t2 with
    | Empty, t | t, Empty -> t
    | Node (a1, l, r, _), Node (a2, _, _, _) ->
      if a1 > a2
      then merge t2 t1
      else
        let merged = merge r t2 in
        let rank_left = rank l and rank_right = rank merged in
        if rank_left >= rank_right
        then Node (a1, l, merged, succ rank_right)
        else Node (a1, merged, l, succ rank_left)

  let insert a t = merge (singleton a) t

  let get_min = function
    | Empty -> Error "empty"
    | Node (a, _, _, _) -> Ok a

  let delete_min = function
    | Empty -> Error "empty"
    | Node (_, l, r, _) -> Ok (merge l r)

  let of_list = function
    | [] -> Empty
    | hd :: tl -> List.fold_left (fun acc x -> insert x acc) (singleton hd) tl

  let uncons (t : 'a t) : ('a option * 'a t) =
    match t with
    | Empty -> (None, Empty)
    | _ ->
      let min = get_min node in
      let rest = delete_min node in
      match min, rest with
      | Ok a, Ok rest -> (Some a, rest)
      | Ok a, Error _ -> (Some a, Empty)
      | _, _ -> (None, Empty)

  let rec to_seq (t : 'a t) () =
    match t with
    | Empty -> Seq.Nil
    | node ->
      let min = get_min node in
      let rest = delete_min node in
      match min, rest with
      | Ok a, Ok rest -> Seq.Cons (a, to_seq rest)
      | Ok a, Error _ -> Seq.Cons (a, to_seq Empty)
      | _, _ -> Seq.Nil

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

(* end *)
let take (n : int) (list : 'a list) : 'a list =
  let rec aux acc count =
    function
    | [] -> acc
    | x :: xs ->
      if count > n
      then acc
      else aux (x :: acc) (succ count) xs
  in
  aux [] 1 list
  |> List.rev

let rec exists_n_in_a_row (n : int) (list : 'a list) : bool =
  match list with
  | hd :: tl when (List.length list) > n ->
    if List.for_all ((=) hd) (take n list)
    then true
    else exists_n_in_a_row n tl
  | _ -> false

module Dijkstra = struct

  let neighboors_of_node (node : Node.t) (nodes : Node.t list) : ((Node.t * direction) list) =
    let nodes_coords = (nodes |> List.map (fun (n : Node.t) -> n.coord)) in
    let neighboors_coords = neighboors_of_coord node.coord nodes_coords in
    List.filter_map (fun (n : Node.t) ->
        let* dir = List.assoc_opt n.coord neighboors_coords in
        Some (n, dir)
      ) nodes

  let shortest_path (starter_node : Node.t) (nodes : Node.t list) =
    let initial_distances = List.map (fun n ->
        if n = starter_node
        then (n, 0)
        else (n, inf)
      ) nodes
    in
    let initial_directions = List.map (fun n -> (n, [])) nodes in
    let queue = PQueue.empty |> PQueue.enqueue (Fun.const 0) starter_node in
    let rec loop
        (distances : (Node.t * int) list)
        (directions : (Node.t * (direction list)) list)
      =
      function
      | [] -> (distances, directions)
      | (queue : Node.t PQueue.t) ->
        let (cur_opt, (queue' : Node.t PQueue.t)) = PQueue.dequeue queue in
        let cur = Option.get cur_opt in
        let neighboors_of_cur = neighboors_of_node cur nodes in
        let (distances', directions', queue') =
          neighboors_of_cur
          |> List.fold_left (fun (distance_acc, directions_acc, queue) ((n : Node.t), dir) ->
              let get_distance node =
                List.assoc_opt node distances
                |> Option.value ~default:(inf)
              in
              let n_directions' =
                List.assoc_opt cur directions
                |> function | Some a -> dir :: a | None -> failwith "No directions?!"
              in
              let is_valid_direction =
                if exists_n_in_a_row 4 n_directions' then false else true
              in
              let alt = n.weight + get_distance cur in
              if (alt <= get_distance n) && is_valid_direction
              then
                let distances' = update (fun (k, v) -> k) (n, alt) distance_acc in
                let directions' =
                  update (fun (k, v) -> k) (n, n_directions') directions_acc
                  |> List.map (fun (a, ds) -> (a, take 4 ds))
                in
                let queue = PQueue.enqueue (Fun.const alt) n queue in
                (distances', directions', queue)
              else (distance_acc, directions_acc, queue)
            ) (distances, directions, queue')
        in
        loop distances' directions' queue'
    in
    loop initial_distances initial_directions queue

end

let main_2 lines =
  let nodes = Node.nodes_from_lines lines in
  let starter_node = nodes |> List.find (fun (n : Node.t) -> n.coord = (0, 0)) in
  let end_node = nodes |> auto_fold (fun (a:Node.t) (b:Node.t) ->
      if a.coord > b.coord then a else b
    )
  in
  let (paths, directions) = Dijkstra.shortest_path starter_node nodes in
  paths |> List.filter (fun ((n : Node.t),_) -> n.coord = end_node.coord)
  |> List.hd
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

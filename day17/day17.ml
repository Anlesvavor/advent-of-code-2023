
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

module Dijkstra = struct
  (* type distances = (node * int) list *)

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
    neighboors_coords
    |> List.map (fun (coord, dir) ->
        List.filter_map (fun (n : Node.t) ->
            if n.coord = coord
            then Some (n, dir)
            else None
          ) nodes
      )

  let shortest_path (starter_node : Node.t) (nodes : Node.t list) : int =
    let initial_distances = List.map (fun n ->
        if n = starter_node
        then (n, 0)
        else (n, inf)
      ) nodes
    in
    let queue = PQueue.queue_of_list (fun n -> n.weight) nodes in
    let initial_visited = [] in
    let rec loop (distances : (Node.t * int) list) =
      function
      | [] -> (distances)
      | (queue : Node.t PQueue.t) ->
        let min_node_of_dist = auto_fold (fun acc n ->
            if (snd acc) < (snd n)
            then acc
            else n
          ) distances
        in
        let (cur_opt, (queue' : Node.t PQueue.t)) = PQueue.pop_assoc min_node_of_dist queue in
        let cur = Option.value cur_opt ~default:(failwith "cur_opt was None!") in
        let neighboors_of_cur = neighboors_of_node cur nodes in
        let neighboors_of_cur_still_in_q =
          neighboors_of_cur
          |> List.filter (fun ((n : Node.t), _) ->
              mem_proj (fst) n queue'
            )
        in
        let distances' =
          neighboors_of_cur_still_in_q
          |> List.fold_left (fun distance_acc (n, dir) ->
              let get_distance node =
                List.assoc_opt node distances
                |> Option.value ~default:(failwith "No distance found for node")
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

(* module Graph = struct *)
(*   module NodeSet = Set.Make(Node) *)
(*   module NeighboorMap = Map.Make(Node) *)
(*   type neighboorValue = ((int * int) * direction) list *)
(*   type t = *)
(*     { nodes : NodeSet.t *)
(*     ; neighboors :  neighboorValue NeighboorMap.t *)
(*     } *)

(*   let graph_of_board (nodes : Node.t list) : t = *)
(*     let max_row = List.fold_left (fun acc (node : Node.t) -> *)
(*         max acc (fst node.coord)) 0 nodes *)
(*     in *)
(*     let max_col = List.fold_left (fun acc (node : Node.t) -> *)
(*         max acc (snd node.coord)) 0 nodes *)
(*     in *)
(*     let neighboors_coords_of_node (node : Node.t) = *)
(*       let (row, col) = node.coord in *)
(*       let e = *)
(*         let* col' = bounded 0 max_col (succ col) in *)
(*         Some ((row, col'), East) *)
(*       in *)
(*       let s = *)
(*         let* row' = bounded 0 max_row (succ row) in *)
(*         Some ((row', col), South) *)
(*       in *)
(*       let w = *)
(*         let* col' = bounded 0 max_col (pred col) in *)
(*         Some ((row, col'), West) *)
(*       in *)
(*       let n = *)
(*         let* row' = bounded 0 max_row (pred row) in *)
(*         Some ((row', col), North) *)
(*       in *)
(*       List.fold_right (cons_opt) [n;e;s;w] [] *)
(*     in *)
(*     let neighboors = nodes |> List.fold_left (fun (acc : neighboorValue NeighboorMap.t) node -> *)
(*         let neighboors = neighboors_coords_of_node node in *)
(*         NeighboorMap.add node neighboors acc *)
(*       ) NeighboorMap.empty *)
(*     in *)
(*     let nodes = NodeSet.of_list nodes in *)
(*     { nodes; neighboors } *)

(*   let neighboors_with_dir_of_node (node : Node.t) (t : t) : (Node.t * direction) list = *)
(*     let coords : ((int * int) * direction) list = NeighboorMap.find node (t.neighboors) in *)
(*     let nodes : NodeSet.t = t.nodes in *)
(*     coords *)
(*     |> List.fold_left (fun acc (coord, direction) -> *)
(*         (\* Refactor fo not use Linear Search *\) *)
(*         let found_node = *)
(*           NodeSet.filter (fun node -> node.coord = coord) nodes *)
(*           |> NodeSet.choose_opt in *)
(*         match found_node with *)
(*         | Some node -> (node, direction) :: acc *)
(*         | None -> acc *)
(*       ) [] *)

(*   let last_node (t : t) : Node.t = *)
(*     let initial = NodeSet.choose (t.nodes) in *)
(*     NodeSet.fold (fun node acc -> *)
(*         if node > acc *)
(*         then node *)
(*         else acc *)
(*       ) (t.nodes) initial *)

(* end *)

(* module Pathfinding = struct *)
(*   let is_reverse a b = *)
(*     match a, b with *)
(*     | North, South | South, North -> true *)
(*     | East, West | West, East -> true *)
(*     | _, _ -> false *)

(*   let no_consecutive_rule (d : direction) (d0 : direction) (d1 : direction) (d2 : direction) : bool = *)
(*     match d, d0, d1, d2 with *)
(*     | a, b, c, f when (a = b) && (b = c) && (c = f) -> false *)
(*     | a, b, _, _ -> not (is_reverse a b) *)

(*   let is_direction_list_valid (directions : direction list) : bool = *)
(*     match directions with *)
(*     | [] -> true *)
(*     | (hd :: tl) as path -> *)
(*       let rec aux = *)
(*         function *)
(*         | a :: ((b :: c :: d :: _) as tl) -> *)
(*           if no_consecutive_rule a b c d *)
(*           then false *)
(*           else if is_reverse a b *)
(*           then false *)
(*           else aux tl *)
(*         | a :: ((b :: _) as tl) -> *)
(*           if is_reverse a b *)
(*           then false *)
(*           else aux tl *)
(*         | _ -> true *)
(*       in *)
(*       aux path *)

(*   let is_node_valid (node : Node.t) : bool = *)
(*     is_direction_list_valid @@ Node.directions_of_path node *)

(*   let is_valid_given_direction (node : Node.t) (dir : direction) : bool = *)
(*     let directions = Node.directions_of_path node in *)
(*     let directions = dir :: directions in *)
(*     is_direction_list_valid directions *)

(*   let get_visit_index (graph : Graph.t) : int = *)
(*     Graph.NodeSet.elements graph.nodes *)
(*     |> List.map (fun (node : Node.t) -> node.visit_count) *)
(*     |> function *)
(*     | hd :: tl ->  List.fold_left (min) hd tl *)
(*     | [] -> 0 *)
(*   (\* |> function *\) *)
(*   (\* | [] -> failwith "?!?!??!?!?!??!" *\) *)
(*   (\* | [(n : Node.t)] -> n.visit_count *\) *)
(*   (\* | hd :: tl -> *\) *)

(*   (\*   let node = List.fold_left (fun (acc : Node.t) (node : Node.t) -> *\) *)
(*   (\*       if acc.visit_count > node.visit_count *\) *)
(*   (\*       then node *\) *)
(*   (\*       else acc *\) *)
(*   (\*     ) hd tl *\) *)
(*   (\*   in *\) *)
(*   (\*   node.visit_count *\) *)

(*   let node_with_lowest_score (graph : Graph.t) : Node.t = *)
(*     let current_visit_index = get_visit_index graph in *)
(*     Graph.NodeSet.elements graph.nodes *)
(*     |> List.filter (fun (node : Node.t) -> node.visit_count = current_visit_index) *)
(*     |> List.filter (fun (node : Node.t) -> is_node_valid node) *)
(*     |> function *)
(*     | hd :: tl -> *)
(*       List.fold_left (fun (acc: Node.t) (node : Node.t) -> *)
(*           if node.visit_count > current_visit_index *)
(*           then acc *)
(*           else if node.score < acc.score *)
(*           then node *)
(*           else acc *)
(*         ) hd tl *)
(*     | [a] -> a *)
(*     | [] -> failwith "No nodes?!" *)

(*   let calculate_score (cur_node : Node.t) (next_node : Node.t) : int = *)
(*     if cur_node.score = inf *)
(*     then inf *)
(*     else cur_node.score + next_node.weight *)


(*   let algorithm (graph : Graph.t) (start_node : Node.t) (target_node : Node.t) = *)
(*     let start_node = Graph.NodeSet.find start_node graph.nodes in *)
(*     let nodes = Graph.NodeSet.map (fun node -> *)
(*         if node.coord = start_node.coord *)
(*         then {node with score = 0} *)
(*         else node *)
(*       ) graph.nodes *)
(*     in *)
(*     let graph = {graph with nodes=nodes } in *)
(*     let rec loop graph = *)
(*       let visit_index = get_visit_index graph in *)
(*       let () = print_endline @@ string_of_int visit_index in *)
(*       let cur = node_with_lowest_score graph in *)
(*       let cur = { cur with visit_count = (succ cur.visit_count) } in *)
(*       let nodes = Graph.NodeSet.map (fun node -> *)
(*           if node.coord = cur.coord *)
(*           then cur *)
(*           else node *)
(*         ) graph.nodes *)
(*       in *)
(*       (\* Use neigh as a function *\) *)
(*       let neighbors = Graph.NeighboorMap.find cur graph.neighboors in *)
(*       let nodes = Graph.NodeSet.map (fun node -> *)
(*           match List.assoc_opt node.coord neighbors with *)
(*           | Some dir when ((node.visit_count <= visit_index) && (is_valid_given_direction node dir)) -> *)
(*             let () = print_endline @@ *)
(*               (string_of_int node.visit_count) ^ " " *)
(*               ^ (Node.string_of_node node) *)
(*               ^ (string_of_direction dir) *)
(*               ^ (Node.string_of_path node) *)
(*             in *)
(*             let new_score = calculate_score cur node in *)
(*             if (new_score < node.score) *)
(*             then { node with score = new_score; route_to_node = Some (cur, dir) } *)
(*             else node *)
(*           | _ -> node *)
(*         ) nodes *)
(*       in *)
(*       let graph = { graph with nodes=nodes } in *)
(*       if cur.coord = target_node.coord *)
(*       then Result.Ok cur *)
(*       else if (node_with_lowest_score graph).score = inf *)
(*       then loop graph *)
(*       else loop graph *)
(*     in *)
(*     loop graph *)

(* end *)


let main lines =
  let nodes = Node.nodes_from_lines lines in
  let graph = Graph.graph_of_board nodes in
  (* let starter_node = NodeSet.find_first (fun elt -> elt.coord = (0, 0)) graph.nodes in *)
  let starter_node = Graph.NodeSet.min_elt (graph.nodes) in
  let target_node = Graph.last_node graph in
  Pathfinding.algorithm graph starter_node target_node
  |> function
  | Ok node ->
    Node.string_of_path node
  (* |> Seq.map (fun ((node : Node.t), _) -> node.weight) *)
  (* |> List.of_seq *)
  (* |> List.rev *)
  (* |> Seq.fold_left (+) 0 *)
  | Error e -> e

let main_2 lines =
  let nodes = Node.nodes_from_lines lines in
  let graph = Graph.graph_of_board nodes in
  (* let starter_node = NodeSet.find_first (fun elt -> elt.coord = (0, 0)) graph.nodes in *)
  let starter_node = Graph.NodeSet.min_elt (graph.nodes) in
  let target_node = Graph.last_node graph in
  Pathfinding.algorithm graph starter_node target_node
  |> function
  | Ok node ->
    Node.string_of_path node
  (* |> Seq.map (fun ((node : Node.t), _) -> node.weight) *)
  (* |> List.of_seq *)
  (* |> List.rev *)
  (* |> Seq.fold_left (+) 0 *)
  | Error e -> e

let () =
  (* let lines = In_channel.input_lines (In_channel.open_text  "day17.txt") in *)
  let lines = In_channel.input_lines (In_channel.open_text "day17.example.txt") in
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  print_endline (main lines)
(* (main lines) |> List.iter (print_int) *)
(* print_int @@ main lines *)

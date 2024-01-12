
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

let manhattan_distance (a_row, a_col) (b_row, b_col) =
  (abs (b_row - a_row)) + (abs (b_col - a_col))

type direction = North | East | South | West

module Node = struct
  type t =
    { coord : int * int
    ; weight : int
    ; score : int
    ; visited : bool
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
            ; score = Int.max_int / 2
            ; visited = false
            ; route_to_node = None
            }
          )
      )
    |> List.flatten

  let distance (a : t) (b : t) : int =
    manhattan_distance (a.coord) (b.coord)

  let rec traverse_path (t : t) () =
    match t.route_to_node with
    | None -> Seq.Nil
    | Some (next, dir) -> Seq.Cons ((next, dir), traverse_path next)

end


module Graph = struct
  module NodeSet = Set.Make(Node)
  module NeighboorMap = Map.Make(Node)
  type neighboorValue = ((int * int) * direction) list
  type t =
    { nodes : NodeSet.t
    ; neighboors :  neighboorValue NeighboorMap.t
    }

  let graph_of_board (nodes : Node.t list) : t =
    let max_row = List.fold_left (fun acc (node : Node.t) ->
        max acc (fst node.coord)) 0 nodes
    in
    let max_col = List.fold_left (fun acc (node : Node.t) ->
        max acc (snd node.coord)) 0 nodes
    in
    let neighboors_coords_of_node (node : Node.t) =
      let (row, col) = node.coord in
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
    in
    let neighboors = nodes |> List.fold_left (fun (acc : neighboorValue NeighboorMap.t) node ->
        let neighboors = neighboors_coords_of_node node in
        NeighboorMap.add node neighboors acc
      ) NeighboorMap.empty
    in
    let nodes = NodeSet.of_list nodes in
    { nodes; neighboors }

  let neighboors_with_dir_of_node (node : Node.t) (t : t) : (Node.t * direction) list =
    let coords : ((int * int) * direction) list = NeighboorMap.find node (t.neighboors) in
    let nodes : NodeSet.t = t.nodes in
    coords
    |> List.fold_left (fun acc (coord, direction) ->
        (* Refactor fo not use Linear Search *)
        let found_node =
          NodeSet.filter (fun node -> node.coord = coord) nodes
          |> NodeSet.choose_opt in
        match found_node with
        | Some node -> (node, direction) :: acc
        | None -> acc
      ) []

  let last_node (t : t) : Node.t =
    let initial = NodeSet.choose (t.nodes) in
    NodeSet.fold (fun node acc ->
        if node > acc
        then node
        else acc
      ) (t.nodes) initial

end

module Pathfinding = struct

  let node_with_lowest_score (graph : Graph.t) : Node.t =
    Graph.NodeSet.elements graph.nodes
    |> List.filter (fun (node : Node.t) -> not node.visited)
    |> function
    | hd :: tl ->
      List.fold_left (fun (acc: Node.t) (node : Node.t) ->
          if node.visited
          then acc
          else if node.score < acc.score
          then node
          else acc
        ) hd tl
    | [] -> failwith "No nodes?!"

  let calculate_score (cur_node : Node.t) (next_node : Node.t) : int =
    if cur_node.score = (Int.max_int / 2)
    then (Int.max_int / 2)
    else cur_node.score + next_node.weight

  let is_valid (node : Node.t) (dir : direction) : bool =
    let is_reverse a b =
      match a, b with
      | North, South | South, North -> true
      | East, West | West, East -> true
      | _, _ -> false
    in
    let rule (d : direction) (d0 : direction) (d1 : direction) : bool =
      match d, d0, d1 with
      | a, b, c when a = b && b = c -> false
      | a, b, _ -> not (is_reverse a b)
    in
    let path =
      Node.traverse_path node
      |> Seq.take 2
      |> Seq.map (snd)
      |> List.of_seq
    in
    match path with
    | [a; b] -> rule dir a b
    | [a] -> not(is_reverse dir a)
    | _ -> true

  let algorithm (graph : Graph.t) (start_node : Node.t) (target_node : Node.t) =
    let start_node = Graph.NodeSet.find start_node graph.nodes in
    let nodes = Graph.NodeSet.map (fun node ->
        if node.coord = start_node.coord
        then {node with score = 0}
        else node
      ) graph.nodes
    in
    let graph = {graph with nodes=nodes } in
    let rec loop graph =
      let cur = node_with_lowest_score graph in
      let cur = { cur with visited = true } in
      let nodes = Graph.NodeSet.map (fun node ->
          if node.coord = cur.coord
          then cur
          else node
        ) graph.nodes
      in
      let neighbors = Graph.NeighboorMap.find cur graph.neighboors in
      let nodes = Graph.NodeSet.map (fun node ->
          match List.assoc_opt node.coord neighbors with
          | Some dir when ((not node.visited) && (is_valid node dir)) ->
            let new_score = calculate_score cur node in
            if (new_score < node.score)
            then { node with score = new_score; route_to_node = Some (cur, dir) }
            else node
          | _ -> node
        ) nodes
      in
      let graph = { graph with nodes=nodes } in
      if cur.coord = target_node.coord
      then Result.Ok cur
      else if (node_with_lowest_score graph).score = (Int.max_int / 2)
      then Result.Error "Path not found : ("
      else loop graph
    in
    loop graph

end

let main lines =
  let nodes = Node.nodes_from_lines lines in
  let graph = Graph.graph_of_board nodes in
  (* let starter_node = NodeSet.find_first (fun elt -> elt.coord = (0, 0)) graph.nodes in *)
  let starter_node = Graph.NodeSet.min_elt (graph.nodes) in
  let target_node = Graph.last_node graph in
  Pathfinding.algorithm graph starter_node target_node
  |> function
  | Ok node ->
    Node.traverse_path node
    |> Seq.map (fun ((node : Node.t), _) -> node.weight)
    |> List.of_seq
    |> List.rev
  (* |> Seq.fold_left (+) 0 *)
  | Error e -> failwith e

let () =
  (* let lines = In_channel.input_lines (In_channel.open_text  "day17.txt") in *)
  let lines = In_channel.input_lines (In_channel.open_text "day17.example.txt") in
  let () = List.iter (Printf.printf "%s\n") lines in
  let () = print_newline () in
  (main lines) |> List.iter (print_int)
(* print_int @@ main lines *)

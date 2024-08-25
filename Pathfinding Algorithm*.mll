(* A* algorithm for pathfinding on a grid *)
type point = {x: int; y: int}
type grid = int array array
type node = {pos: point; g: int; h: int; f: int; parent: node option}

let heuristic p1 p2 =
  abs (p1.x - p2.x) + abs (p1.y - p2.y)

let get_neighbors grid {pos={x; y}; _} =
  let neighbors = ref [] in
  for dx = -1 to 1 do
    for dy = -1 to 1 do
      if dx <> 0 || dy <> 0 then
        let nx = x + dx in
        let ny = y + dy in
        if nx >= 0 && ny >= 0 && nx < Array.length grid && ny < Array.length grid.(0) then
          neighbors := {pos = {x = nx; y = ny}; g = 0; h = 0; f = 0; parent = None} :: !neighbors
    done
  done;
  !neighbors

let astar_search grid start goal =
  let open List in
  let open Hashtbl in
  let open Queue in
  let open Set in
  let closed_set = create 100 in
  let open_set = Queue.create () in
  let came_from = create 100 in
  let start_node = {pos = start; g = 0; h = heuristic start goal; f = 0; parent = None} in
  Queue.add start_node open_set;
  let rec search () =
    if Queue.is_empty open_set then
      None
    else
      let current = Queue.take open_set in
      if current.pos = goal then
        Some current
      else
        let neighbors = get_neighbors grid current in
        List.iter (fun neighbor ->
          if not (mem neighbor.pos closed_set) then
            let tentative_g = current.g + 1 in
            let tentative_f = tentative_g + heuristic neighbor.pos goal in
            let open_node = {neighbor with g = tentative_g; h = heuristic neighbor.pos goal; f = tentative_f; parent = Some current} in
            if not (mem open_node.pos closed_set) then
              Queue.add open_node open_set;
              add open_node.pos closed_set
        ) neighbors;
        search ()
  in
  search ()

(* Example usage *)
let () =
  let grid = Array.make_matrix 10 10 0 in
  let start = {x = 0; y = 0} in
  let goal = {x = 9; y = 9} in
  match astar_search grid start goal with
  | Some node -> Printf.printf "Path found to goal\n"
  | None -> Printf.printf "No path found\n"

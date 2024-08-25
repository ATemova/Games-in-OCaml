(* The code below solves the maximum flow problem in a flow network using the Edmonds-Karp algorithm *)

module Graph = struct
  (* Define the type for the graph as an adjacency matrix *)
  type t = {
    capacity: int array array;  (* Capacity matrix *)
    mutable flow: int array array;  (* Flow matrix *)
    num_vertices: int;  (* Number of vertices *)
  }

  (* Create a new graph with n vertices *)
  let create n =
    let capacity = Array.make_matrix n n 0 in
    let flow = Array.make_matrix n n 0 in
    { capacity; flow; num_vertices = n }

  (* Add an edge with capacity to the graph *)
  let add_edge g u v cap =
    g.capacity.(u).(v) <- cap

  (* Find an augmenting path using BFS *)
  let bfs g source sink parent =
    let visited = Array.make g.num_vertices false in
    let queue = Queue.create () in
    Queue.add source queue;
    visited.(source) <- true;
    let rec bfs' () =
      if Queue.is_empty queue then false
      else
        let u = Queue.take queue in
        if u = sink then true
        else
          let rec visit_neighbors v =
            if v < g.num_vertices then
              if not visited.(v) && g.capacity.(u).(v) - g.flow.(u).(v) > 0 then
                (Queue.add v queue;
                 visited.(v) <- true;
                 parent.(v) <- u;
                 visit_neighbors (v + 1))
              else
                visit_neighbors (v + 1)
          in
          visit_neighbors 0;
          bfs' ()
    in
    bfs' ()

  (* Calculate the maximum flow in the graph from source to sink *)
  let edmonds_karp g source sink =
    let parent = Array.make g.num_vertices (-1) in
    let rec augment_flow path_flow =
      if path_flow = 0 then ()
      else
        let rec update_flow u =
          if parent.(u) >= 0 then
            let p = parent.(u) in
            g.flow.(p).(u) <- g.flow.(p).(u) + path_flow;
            g.flow.(u).(p) <- g.flow.(u).(p) - path_flow;
            update_flow p
        in
        update_flow sink
    in
    let rec max_flow total_flow =
      if bfs g source sink parent then
        let path_flow = ref max_int in
        let rec find_path_flow u =
          if parent.(u) = -1 then
            path_flow := min !path_flow (g.capacity.(parent.(u)).(u) - g.flow.(parent.(u)).(u))
          else
            find_path_flow parent.(u)
        in
        find_path_flow sink;
        augment_flow !path_flow;
        max_flow (total_flow + !path_flow)
      else
        total_flow
    in
    max_flow 0
end

(* Example usage *)
let () =
  (* Create a graph with 6 vertices *)
  let g = Graph.create 6 in

  (* Add edges with capacities *)
  Graph.add_edge g 0 1 16;
  Graph.add_edge g 0 2 13;
  Graph.add_edge g 1 2 10;
  Graph.add_edge g 1 3 12;
  Graph.add_edge g 2 1 4;
  Graph.add_edge g 2 4 14;
  Graph.add_edge g 3 2 9;
  Graph.add_edge g 3 5 20;
  Graph.add_edge g 4 3 7;
  Graph.add_edge g 4 5 4;

  (* Compute the maximum flow from source (0) to sink (5) *)
  let max_flow = Graph.edmonds_karp g 0 5 in
  Printf.printf "The maximum flow from source to sink is %d\n" max_flow

type 'a ast =
  | Node of 'a * 'a ast list
  | Leaf of 'a

let rec traverse_bottom_up f = function
  | Leaf x -> f (Leaf x)
  | Node (x, children) ->
    let transformed_children = List.map (traverse_bottom_up f) children in
    f (Node (x, transformed_children))

let rec traverse_top_down f node =
  let transformed_node = f node in
  match transformed_node with
  | Leaf _ -> transformed_node
  | Node (x, children) ->
    let transformed_children = List.map (traverse_top_down f) children in
    Node (x, transformed_children)

let rec transform ast rules =
  let apply_rule node =
    List.find_map (fun rule -> rule node) rules
  in
  traverse_bottom_up (fun node -> apply_rule node |> Option.value ~default:node) ast

let rec match_pattern pattern ast =
  match (pattern, ast) with
  | Leaf x, Leaf y when x = y -> true
  | Node (x, p_children), Node (y, a_children) when x = y ->
    List.for_all2 match_pattern p_children a_children
  | _ -> false

(* AVL Tree implementation *)
type 'a avl_tree =
  | Empty
  | Node of 'a * 'a avl_tree * 'a avl_tree * int

let height = function
  | Empty -> 0
  | Node (_, _, _, h) -> h

let balance_factor = function
  | Empty -> 0
  | Node (_, left, right, _) -> height left - height right

let create_node value left right =
  let h = 1 + max (height left) (height right) in
  Node (value, left, right, h)

let rotate_left = function
  | Node (x, a, Node (y, b, c, _), _) -> create_node y (create_node x a b) c
  | t -> t

let rotate_right = function
  | Node (y, Node (x, a, b, _), c, _) -> create_node x a (create_node y b c)
  | t -> t

let balance tree =
  match tree with
  | Node (value, left, right, _) as node ->
    if balance_factor node = 2 then
      if balance_factor left < 0 then
        rotate_left (create_node value (rotate_right left) right)
      else
        rotate_left node
    else if balance_factor node = -2 then
      if balance_factor right > 0 then
        rotate_right (create_node value left (rotate_left right))
      else
        rotate_right node
    else
      node
  | Empty -> Empty

let rec insert value = function
  | Empty -> Node (value, Empty, Empty, 1)
  | Node (v, left, right, h) as tree ->
    if value < v then
      balance (create_node v (insert value left) right)
    else if value > v then
      balance (create_node v left (insert value right))
    else
      tree

(* Example usage *)
let () =
  let tree = Empty in
  let tree = insert 10 (insert 20 (insert 5 tree)) in
  Printf.printf "AVL Tree balanced and nodes inserted.\n"

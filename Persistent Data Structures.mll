(*Persistent BST*)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let rec insert x t = match t with
  | Empty -> Node (Empty, x, Empty)
  | Node (l, v, r) when x < v -> Node (insert x l, v, r)
  | Node (l, v, r) when x > v -> Node (l, v, insert x r)
  | _ -> t

let rec delete x t = match t with
  | Empty -> Empty
  | Node (l, v, r) when x < v -> Node (delete x l, v, r)
  | Node (l, v, r) when x > v -> Node (l, v, delete x r)
  | Node (l, _, r) -> merge_trees l r

and merge_trees l r = match (l, r) with
  | Empty, r -> r
  | l, Empty -> l
  | _ -> let (m, r') = remove_min r in Node (l, m, r')

and remove_min = function
  | Empty -> failwith "Tree is empty"
  | Node (Empty, v, r) -> (v, r)
  | Node (l, v, r) -> let (m, l') = remove_min l in (m, Node (l', v, r))
(*versioning and history*)
type 'a persistent_tree = {
  tree: 'a tree;
  history: 'a tree list
}

let empty_persistent_tree = { tree = Empty; history = [] }

let insert_persistent x pt =
  let new_tree = insert x pt.tree in
  { tree = new_tree; history = pt.tree :: pt.history }

let delete_persistent x pt =
  let new_tree = delete x pt.tree in
  { tree = new_tree; history = pt.tree :: pt.history }

let revert pt = match pt.history with
  | [] -> pt
  | h :: rest -> { tree = h; history = rest }

(* Trie data structure *)
type trie_node = {
  mutable children: trie_node list;
  mutable is_end_of_word: bool;
}

let create_node () = {
  children = [];
  is_end_of_word = false;
}

let insert root word =
  let rec insert_char node chars =
    match chars with
    | [] -> node.is_end_of_word <- true
    | char :: rest ->
      let child = List.find_opt (fun c -> c.is_end_of_word = char) node.children in
      let child = match child with
        | Some c -> c
        | None ->
          let new_node = create_node () in
          node.children <- new_node :: node.children;
          new_node
      in
      insert_char child rest
  in
  insert_char root (List.of_seq (String.to_seq word))

let search root word =
  let rec search_char node chars =
    match chars with
    | [] -> node.is_end_of_word
    | char :: rest ->
      let child = List.find_opt (fun c -> c.is_end_of_word = char) node.children in
      match child with
      | Some c -> search_char c rest
      | None -> false
  in
  search_char root (List.of_seq (String.to_seq word))

(* Example usage *)
let () =
  let root = create_node () in
  insert root "hello";
  insert root "world";
  Printf.printf "Search 'hello': %b\n" (search root "hello");
  Printf.printf "Search 'world': %b\n" (search root "world");
  Printf.printf "Search 'foo': %b\n" (search root "foo")

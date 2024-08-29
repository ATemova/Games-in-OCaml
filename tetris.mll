(* Due to complexity, this is a highly simplified example *)

type shape = I | O | T | S | Z | J | L
type tile = Empty | Filled

type grid = tile array array

let create_grid size = Array.make_matrix ~dimx:size ~dimy:size Empty

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun tile ->
      let symbol = match tile with
        | Empty -> "."
        | Filled -> "#"
      in
      print_string symbol
    ) row;
    print_endline ""
  ) grid;
  print_endline ""

let () =
  let grid = create_grid 10 in
  print_endline "Initial grid:";
  print_grid grid;
  (* Implement falling blocks and collision detection *)

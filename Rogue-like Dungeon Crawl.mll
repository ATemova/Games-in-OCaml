type tile = Empty | Wall | Enemy
type grid = tile array array

let create_grid size =
  let grid = Array.make_matrix ~dimx:size ~dimy:size Empty in
  (* Add walls and enemies *)
  grid.(1).(1) <- Wall;
  grid.(2).(2) <- Enemy;
  grid

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun tile ->
      let symbol = match tile with
        | Empty -> "."
        | Wall -> "#"
        | Enemy -> "E"
      in
      print_string symbol
    ) row;
    print_endline ""
  ) grid;
  print_endline ""

let () =
  let size = 5 in
  let grid = create_grid size in
  print_endline "Dungeon Grid:";
  print_grid grid;
  (* Implement movement, interaction, etc. *)

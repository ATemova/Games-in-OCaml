(* A simplified version with movement and a static maze layout *)

type tile = Wall | Empty | Pacman | Ghost
type grid = tile array array

let create_grid size =
  let grid = Array.make_matrix ~dimx:size ~dimy:size Empty in
  grid.(1).(1) <- Wall;
  grid.(2).(2) <- Ghost;
  grid.(3).(3) <- Pacman;
  grid

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun tile ->
      let symbol = match tile with
        | Wall -> "#"
        | Empty -> "."
        | Pacman -> "P"
        | Ghost -> "G"
      in
      print_string symbol
    ) row;
    print_endline ""
  ) grid;
  print_endline ""

let move_pacman grid direction =
  let (pac_x, pac_y) =
    Array.fold_left (fun (px, py) (x, y) ->
      if grid.(x).(y) = Pacman then (x, y) else (px, py)
    ) (0, 0) (Array.init 5 (fun x -> Array.init 5 (fun y -> (x, y))))
  in
  let new_x, new_y =
    match direction with
    | "up" -> (pac_x - 1, pac_y)
    | "down" -> (pac_x + 1, pac_y)
    | "left" -> (pac_x, pac_y - 1)
    | "right" -> (pac_x, pac_y + 1)
    | _ -> (pac_x, pac_y)
  in
  if new_x >= 0 && new_x < 5 && new_y >= 0 && new_y < 5 && grid.(new_x).(new_y) <> Wall then (
    grid.(pac_x).(pac_y) <- Empty;
    grid.(new_x).(new_y) <- Pacman
  )

let () =
  let grid = create_grid 5 in
  print_endline "Initial grid:";
  print_grid grid;
  print_endline "Move (up, down, left, right):";
  let direction = read_line () in
  move_pacman grid direction;
  print_endline "After move:";
  print_grid grid

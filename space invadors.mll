(* Space Invaders is quite complex. Here’s a simplified version with static enemies and player: *)

type entity = Player | Invader | Empty
type grid = entity array array

let create_grid size =
  let grid = Array.make_matrix ~dimx:size ~dimy:size Empty in
  grid.(4).(2) <- Player;
  for i = 0 to 2 do
    grid.(0).(i) <- Invader
  done;
  grid

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun tile ->
      let symbol = match tile with
        | Player -> "P"
        | Invader -> "I"
        | Empty -> "."
      in
      print_string symbol
    ) row;
    print_endline ""
  ) grid;
  print_endline ""

let move_player grid direction =
  let (player_x, player_y) =
    Array.fold_left (fun (px, py) (x, y) ->
      if grid.(x).(y) = Player then (x, y) else (px, py)
    ) (0, 0) (Array.init 5 (fun x -> Array.init 5 (fun y -> (x, y))))
  in
  let new_x, new_y =
    match direction with
    | "left" -> (player_x, player_y - 1)
    | "right" -> (player_x, player_y + 1)
    | _ -> (player_x, player_y)
  in
  if new_y >= 0 && new_y < 5 then (
    grid.(player_x).(player_y) <- Empty;
    grid.(new_x).(new_y) <- Player
  )

let () =
  let grid = create_grid 5 in
  print_grid grid;
  print_endline "Move left or right:";
  let direction = read_line () in
  move_player grid direction;
  print_grid grid

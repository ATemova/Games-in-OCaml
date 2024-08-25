(* Define dungeon layout, player, and enemies *)
type tile = Empty | Wall | Floor
type dungeon = tile array array
type player = { position : int * int; health : int }
type enemy = { position : int * int; health : int }

(* Generate a dungeon with rooms and corridors *)
let generate_dungeon () =
  let dungeon = Array.make_matrix 50 50 Wall in
  (* Implement room and corridor generation here *)
  dungeon

(* Move the player in the dungeon *)
let move_player player direction dungeon =
  let (x, y) = player.position in
  match direction with
  | "up" when x > 0 && dungeon.(x - 1).(y) <> Wall -> { player with position = (x - 1, y) }
  | "down" when x < 49 && dungeon.(x + 1).(y) <> Wall -> { player with position = (x + 1, y) }
  | "left" when y > 0 && dungeon.(x).(y - 1) <> Wall -> { player with position = (x, y - 1) }
  | "right" when y < 49 && dungeon.(x).(y + 1) <> Wall -> { player with position = (x, y + 1) }
  | _ -> player

(* Main game loop *)
let rec game_loop player dungeon =
  (* Implement game logic: handle player input, move enemies, etc. *)
  ()

(* Main function to start the game *)
let () =
  let dungeon = generate_dungeon () in
  let player = { position = (25, 25); health = 100 } in
  game_loop player dungeon

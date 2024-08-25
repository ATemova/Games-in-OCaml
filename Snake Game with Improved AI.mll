(* Advanced Snake Game with AI for movement *)

type direction = Up | Down | Left | Right
type position = int * int
type game_state = {
  snake : position list;
  food : position;
  direction : direction;
  score : int;
}

let init_game () = {
  snake = [(10, 10)];
  food = (5, 5);
  direction = Right;
  score = 0;
}

let move_snake state =
  (* Update snake position based on direction and AI *)
  state

let print_game state =
  (* Print snake and food *)
  ()

let play_game () =
  let state = init_game () in
  (* Implement game loop with AI *)
  print_game state

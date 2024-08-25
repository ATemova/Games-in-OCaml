(* 2048 game with AI for movement *)

type tile = int
type board = tile array array

let init_board () : board =
  Array.make_matrix ~dimx:4 ~dimy:4 0

let print_board board =
  Array.iter (fun row ->
    Array.iter (Printf.printf "%d ") row;
    print_endline ""
  ) board

let move_left board =
  (* Implement left move logic with AI *)
  board

let play_game () =
  let board = init_board () in
  (* Implement game loop with AI *)
  print_board board

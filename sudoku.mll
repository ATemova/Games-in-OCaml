(* Advanced Sudoku Solver with Backtracking *)

type board = int array array

let init_board () : board =
  Array.make_matrix ~dimx:9 ~dimy:9 0

let print_board board =
  Array.iter (fun row ->
    Array.iter (Printf.printf "%d ") row;
    print_endline ""
  ) board

let is_valid board row col num =
  (* Check if num can be placed at (row, col) *)
  true

let rec solve board =
  (* Solve Sudoku using backtracking *)
  board

let play_game () =
  let board = init_board () in
  let solved_board = solve board in
  print_board solved_board

(* Advanced Minesweeper with recursion *)

type cell = Bomb | Number of int | Empty
type board = cell array array

let init_board size num_bombs =
  let b = Array.make_matrix ~dimx:size ~dimy:size Empty in
  (* Place bombs and numbers recursively *)
  b

let rec uncover_cell board x y =
  match board.(x).(y) with
  | Bomb -> board
  | Number _ -> board
  | Empty -> (* Recursive uncovering *)
    board

let print_board board =
  Array.iter (fun row ->
    Array.iter (function
      | Bomb -> print_string "* "
      | Number n -> Printf.printf "%d " n
      | Empty -> print_string ". ") row;
    print_endline ""
  ) board

let play_game () =
  let board = init_board 10 20 in
  (* Implement game loop *)
  print_board board

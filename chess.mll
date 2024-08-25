(* Chess game with simple AI *)

type piece = King | Queen | Rook | Bishop | Knight | Pawn | Empty
type position = int * int
type board = piece array array

let init_board () : board =
  Array.make_matrix ~dimx:8 ~dimy:8 Empty

let print_board board =
  Array.iter (fun row ->
    Array.iter (function
      | King -> print_string "K "
      | Queen -> print_string "Q "
      | Rook -> print_string "R "
      | Bishop -> print_string "B "
      | Knight -> print_string "N "
      | Pawn -> print_string "P "
      | Empty -> print_string ". ") row;
    print_endline ""
  ) board

let move_piece board start_pos end_pos =
  (* Move piece and update board *)
  board

let play_game () =
  let board = init_board () in
  (* Implement game loop with simple AI *)
  print_board board

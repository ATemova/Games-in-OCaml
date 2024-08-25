(* Advanced Tic-Tac-Toe with Minimax Algorithm *)

type player = X | O | Empty
type board = player array array

let init_board () = Array.make_matrix ~dimx:3 ~dimy:3 Empty

let print_board board =
  let print_cell = function
    | X -> print_string "X "
    | O -> print_string "O "
    | Empty -> print_string ". "
  in
  Array.iter (fun row ->
    Array.iter print_cell row;
    print_endline ""
  ) board

let check_winner board =
  let check_line a b c = a = b && b = c && a <> Empty in
  let check_rows () = Array.exists (fun row -> check_line row.(0) row.(1) row.(2)) board in
  let check_cols () =
    let transpose b = Array.init 3 (fun i -> [| b.(0).(i); b.(1).(i); b.(2).(i) |]) in
    Array.exists (fun row -> check_line row.(0) row.(1) row.(2)) (transpose board)
  in
  let check_diagonals () =
    let d1 = check_line board.(0).(0) board.(1).(1) board.(2).(2) in
    let d2 = check_line board.(0).(2) board.(1).(1) board.(2).(0) in
    d1 || d2
  in
  check_rows () || check_cols () || check_diagonals ()

let minimax board is_maximizing =
  let score = if check_winner board then
                if is_maximizing then -10 else 10
              else 0
  in
  if score <> 0 then score
  else
    let moves = (* Generate possible moves *) [] in
    let scores = List.map (fun move -> (* Evaluate each move *) 0) moves in
    if is_maximizing then List.fold_left max (-1000) scores
    else List.fold_left min 1000 scores

let best_move board =
  let moves = (* Generate possible moves *) [] in
  List.fold_left (fun (best_move, best_value) move ->
    let value = minimax (* Apply move *) true in
    if value > best_value then (move, value) else (best_move, best_value)
  ) (None, -1000) moves

let play_game () =
  let board = init_board () in
  (* Implement game loop with AI *)
  print_board board

type cell = Empty | Red | Yellow
type board = cell array array

let create_board () = Array.make_matrix ~dimx:6 ~dimy:7 Empty

let drop_piece board col color =
  let rec drop row =
    if row = 6 then false
    else if board.(row).(col) = Empty then (board.(row).(col) <- color; true)
    else drop (row + 1)
  in
  drop 0

let ai_move board =
  let best_col = ref 0 in
  let best_score = ref (-1) in
  for col = 0 to 6 do
    if drop_piece board col Yellow then
      let score = Random.int 100 in (* Placeholder for real AI logic *)
      if score > !best_score then (
        best_score := score;
        best_col := col
      );
      (* Undo the move *)
      board.(0).(col) <- Empty
    done;
  drop_piece board !best_col Yellow

let () =
  let board = create_board () in
  let rec game_loop turn =
    print_endline "Current board:";
    print_board board;
    match turn with
    | `Player ->
      print_endline "Your turn! Enter column:";
      let col = read_int () in
      if drop_piece board col Red then
        game_loop `AI
      else
        print_endline "Invalid move. Try again."; game_loop `Player
    | `AI ->
      ai_move board;
      game_loop `Player
  in
  game_loop `Player

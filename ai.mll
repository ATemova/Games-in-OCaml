type player = X | O
type cell = Empty | Occupied of player
type board = cell array array

let create_board () = Array.make_matrix ~dimx:3 ~dimy:3 Empty

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      match cell with
      | Empty -> print_string " . "
      | Occupied X -> print_string " X "
      | Occupied O -> print_string " O "
    ) row;
    print_endline ""
  ) board;
  print_endline ""

let make_move board row col player =
  if board.(row).(col) = Empty then
    board.(row).(col) <- Occupied player
  else
    print_endline "Cell already occupied!"

let ai_move board =
  let empty_cells = Array.fold_left (fun acc (r, c) ->
    if board.(r).(c) = Empty then (r, c) :: acc else acc
  ) [] (Array.to_list (Array.init 3 (fun r -> Array.init 3 (fun c -> (r, c))))) in
  if empty_cells <> [] then
    let (r, c) = List.nth empty_cells (Random.int (List.length empty_cells)) in
    make_move board r c O

let check_winner board =
  let check_line a = match Array.to_list a with
    | [Occupied X; Occupied X; Occupied X] -> Some X
    | [Occupied O; Occupied O; Occupied O] -> Some O
    | _ -> None
  in
  let check_row () = Array.fold_left (fun acc row -> match acc with
    | Some _ -> acc
    | None -> check_line row
  ) None board in
  let check_col () = Array.fold_left (fun acc col ->
    if acc <> None then acc
    else check_line (Array.init 3 (fun r -> board.(r).(col)))
  ) None (Array.init 3 (fun c -> c)) in
  let check_diag () =
    let diag1 = Array.init 3 (fun i -> board.(i).(i)) in
    let diag2 = Array.init 3 (fun i -> board.(i).(2 - i)) in
    match check_line diag1, check_line diag2 with
    | Some winner, _ | _, Some winner -> Some winner
    | None, None -> None
  in
  match check_row (), check_col (), check_diag () with
  | Some winner, _, _ | _, Some winner, _ | _, _, Some winner -> Some winner
  | None, None, None -> None

let () =
  Random.self_init ();
  let board = create_board () in
  let rec game_loop current_player =
    print_board board;
    match check_winner board with
    | Some X -> print_endline "Player X wins!"
    | Some O -> print_endline "Player O wins!"
    | None ->
      if Array.exists (Array.exists ((=) Empty)) board then
        if current_player = X then (
          print_endline "Player X's turn:";
          let row = read_int () in
          let col = read_int () in
          make_move board row col X;
          game_loop O
        ) else (
          ai_move board;
          game_loop X
        )
      else
        print_endline "It's a draw!"
  in
  game_loop X

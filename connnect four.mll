type cell = Empty | Red | Yellow
type board = cell array array

let create_board () = Array.make_matrix ~dimx:6 ~dimy:7 Empty

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      let symbol = match cell with
        | Empty -> "."
        | Red -> "R"
        | Yellow -> "Y"
      in
      print_string (symbol ^ " ")
    ) row;
    print_endline ""
  ) board;
  print_endline ""

let drop_piece board col color =
  let rec drop row =
    if row = 6 then false
    else if board.(row).(col) = Empty then (board.(row).(col) <- color; true)
    else drop (row + 1)
  in
  drop 0

let check_winner board color =
  let check_line arr =
    let rec check = function
      | [] | [_] | [_; _] -> false
      | hd1 :: hd2 :: tl ->
        if hd1 = color && hd1 = hd2 then check (hd2 :: tl)
        else false
    in
    check (Array.to_list arr)
  in
  let check_row () =
    Array.exists (fun row -> check_line row) board
  in
  let check_col () =
    Array.exists (fun col -> check_line col) (Array.init 7 (fun i -> Array.init 6 (fun j -> board.(j).(i))))
  in
  let check_diagonal () =
    let diagonals = [] in (* Diagonal checking would be implemented here *)
    List.exists (fun diag -> check_line diag) diagonals
  in
  check_row () || check_col () || check_diagonal ()

let () =
  let board = create_board () in
  let rec game_loop color =
    print_board board;
    print_endline "Enter column to drop piece:";
    let col = read_int () in
    if drop_piece board col color then
      if check_winner board color then
        Printf.printf "%s wins!\n" (match color with Red -> "Red" | Yellow -> "Yellow")
      else
        game_loop (if color = Red then Yellow else Red)
    else
      print_endline "Column full. Try again.";
      game_loop color
  in
  game_loop Red

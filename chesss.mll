type piece = King | Queen | Rook | Bishop | Knight | Pawn
type color = White | Black
type square = Empty | Occupied of piece * color
type board = square array array

let create_board () = Array.make_matrix ~dimx:8 ~dimy:8 Empty

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      match cell with
      | Empty -> print_string " . "
      | Occupied (piece, color) ->
        let piece_char = match piece with
          | King -> 'K'
          | Queen -> 'Q'
          | Rook -> 'R'
          | Bishop -> 'B'
          | Knight -> 'N'
          | Pawn -> 'P'
        in
        let color_char = match color with
          | White -> Char.uppercase_ascii piece_char
          | Black -> Char.lowercase_ascii piece_char
        in
        print_char color_char
    ) row;
    print_endline ""
  ) board;
  print_endline ""

let () =
  let board = create_board () in
  print_board board;
  (* Further implementation would involve setting up pieces, rules, moves, etc. *)

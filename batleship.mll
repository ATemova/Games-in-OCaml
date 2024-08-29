type cell = Empty | Ship | Hit | Miss
type board = cell array array

let create_board size = Array.make_matrix ~dimx:size ~dimy:size Empty

let place_ship board size x y =
  for i = 0 to size - 1 do
    board.(x).(y + i) <- Ship
  done

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      let symbol = match cell with
        | Empty -> "."
        | Ship -> "S"
        | Hit -> "H"
        | Miss -> "M"
      in
      print_string symbol
    ) row;
    print_endline ""
  ) board;
  print_endline ""

let () =
  let size = 5 in
  let board = create_board size in
  place_ship board 3 1 1;
  print_board board;
  (* Add game logic for guessing and handling hits/misses *)

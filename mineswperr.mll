type cell = Empty | Mine | Revealed of int
type board = cell array array

let create_board size mines =
  let board = Array.make_matrix ~dimx:size ~dimy:size Empty in
  let place_mines () =
    let rec place count =
      if count = 0 then ()
      else
        let row = Random.int size in
        let col = Random.int size in
        if board.(row).(col) = Empty then (
          board.(row).(col) <- Mine;
          place (count - 1)
        ) else place count
    in
    place mines
  in
  let count_mines r c =
    let delta = [-1; 0; 1] in
    List.fold_left (fun acc dr ->
      acc + List.fold_left (fun acc dc ->
        if dr = 0 && dc = 0 then acc
        else
          let nr = r + dr in
          let nc = c + dc in
          if nr >= 0 && nr < size && nc >= 0 && nc < size then
            if board.(nr).(nc) = Mine then acc + 1 else acc
          else acc
      ) acc delta
    ) 0 delta
  in
  place_mines ();
  for r = 0 to size - 1 do
    for c = 0 to size - 1 do
      if board.(r).(c) <> Mine then
        board.(r).(c) <- Revealed (count_mines r c)
    done
  done;
  board

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      match cell with
      | Empty -> print_string ". "
      | Mine -> print_string "* "
      | Revealed n -> print_int n; print_string " "
    ) row;
    print_endline ""
  ) board;
  print_endline ""

let () =
  Random.self_init ();
  let size = 10 in
  let mines = 20 in
  let board = create_board size mines in
  print_board board;
  (* Add user interaction for revealing cells and handling game state *)

let solve_sudoku board =
  let is_valid board row col num =
    let check_row r = Array.for_all (fun x -> x <> num) board.(r) in
    let check_col c = Array.for_all (fun r -> r.(c) <> num) board in
    let check_box r c =
      let box_row = r / 3 * 3 in
      let box_col = c / 3 * 3 in
      Array.for_all (fun i ->
        Array.for_all (fun j -> board.(box_row + i).(box_col + j) <> num) [|0; 1; 2|]
      ) [|0; 1; 2|]
    in
    check_row row && check_col col && check_box row col
  in
  let rec solve () =
    try
      let (row, col) =
        Array.fold_left (fun acc r ->
          if acc <> (-1, -1) then acc
          else Array.fold_left (fun acc c ->
            if acc <> (-1, -1) then acc
            else if board.(r).(c) = 0 then (r, c) else acc
          ) acc board.(r)
        ) (-1, -1) board
      in
      if row = -1 then true else
        let rec try_nums nums =
          match nums with
          | [] -> false
          | n::ns ->
            if is_valid board row col n then
              (board.(row).(col) <- n;
               if solve () then true
               else (board.(row).(col) <- 0; try_nums ns))
            else try_nums ns
        in
        try_nums [1; 2; 3; 4; 5; 6; 7; 8; 9]
    with _ -> false
  in
  solve ()

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      print_int cell;
      print_string " "
    ) row;
    print_endline ""
  ) board

let () =
  let board = [|
    [|5; 3; 0; 0; 7; 0; 0; 0; 0|];
    [|6; 0; 0; 1; 9; 5; 0; 0; 0|];
    [|0; 9; 8; 0; 0; 0; 0; 6; 0|];
    [|8; 0; 0; 0; 6; 0; 0; 0; 3|];
    [|4; 0; 0; 8; 0; 3; 0; 0; 1|];
    [|7; 0; 0; 0; 2; 0; 0; 0; 6|];
    [|0; 6; 0; 0; 0; 0; 2; 8; 0|];
    [|0; 0; 0; 4; 1; 9; 0; 0; 5|];
    [|0; 0; 0; 0; 8; 0; 0; 7; 9|]
  |] in
  if solve_sudoku board then
    print_endline "Solved Sudoku:"; print_board board
  else
    print_endline "No solution exists."

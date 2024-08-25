(* Define Sudoku grid and constraints *)
type sudoku = int option array array

(* Check if a number can be placed in a cell *)
let is_valid board row col num =
  let in_row r = Array.for_all (fun x -> x <> num) board.(r) in
  let in_col c = Array.for_all (fun x -> x <> num) (Array.map (fun row -> row.(c)) board) in
  let in_box r c =
    let start_row = (r / 3) * 3 and start_col = (c / 3) * 3 in
    Array.for_all (fun i ->
      Array.for_all (fun j ->
        match board.(start_row + i).(start_col + j) with
        | Some n -> n <> num
        | None -> true
      ) [|0; 1; 2|]
    ) [|0; 1; 2|]
  in
  in_row row && in_col col && in_box row col

(* Recursive backtracking function to solve Sudoku *)
let rec solve board =
  let find_empty () =
    let rec aux r c =
      if r = 9 then None
      else if c = 9 then aux (r + 1) 0
      else match board.(r).(c) with
        | Some _ -> aux r (c + 1)
        | None -> Some (r, c)
    in aux 0 0
  in
  match find_empty () with
  | None -> true
  | Some (row, col) ->
    let rec try_numbers num =
      if num > 9 then false
      else if is_valid board row col num then
        let new_board = Array.map Array.copy board in
        new_board.(row).(col) <- Some num;
        if solve new_board then
          (Array.blit new_board 0 board 0 9; true)
        else try_numbers (num + 1)
      else try_numbers (num + 1)
    in
    try_numbers 1

(* Main function to test the Sudoku solver *)
let () =
  let board = Array.make_matrix 9 9 None in
  (* Fill in some cells for testing *)
  solve board;
  (* Print or visualize the solved board *)

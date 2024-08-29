type cell = Empty | Number of int
type board = cell array array

let create_board () = Array.make_matrix ~dimx:4 ~dimy:4 Empty

let add_random_tile board =
  let empty_cells =
    Array.fold_left (fun acc (x, y) ->
      if board.(x).(y) = Empty then (x, y) :: acc else acc
    ) [] (Array.init 4 (fun x -> Array.init 4 (fun y -> (x, y))))
  in
  if empty_cells <> [] then
    let (x, y) = List.nth empty_cells (Random.int (List.length empty_cells)) in
    board.(x).(y) <- Number (if Random.int 10 = 0 then 4 else 2)

let print_board board =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      let symbol = match cell with
        | Empty -> "."
        | Number n -> string_of_int n
      in
      print_string (symbol ^ " ")
    ) row;
    print_endline ""
  ) board;
  print_endline ""

let () =
  Random.self_init ();
  let board = create_board () in
  add_random_tile board;
  add_random_tile board;
  print_board board;
  (* Implement move and merge logic *)

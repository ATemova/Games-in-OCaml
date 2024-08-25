(* Conway's Game of Life *)
let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun cell -> Printf.printf "%c " (if cell then '#' else '.')) row;
    print_newline ()
  ) grid

let next_state grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let new_grid = Array.make_matrix rows cols false in
  let count_live_neighbors grid x y =
    let count = ref 0 in
    for dx = -1 to 1 do
      for dy = -1 to 1 do
        if (dx <> 0 || dy <> 0) && (x + dx >= 0) && (x + dx < rows) && (y + dy >= 0) && (y + dy < cols) then
          if grid.(x + dx).(y + dy) then incr count
      done
    done;
    !count
  in
  for x = 0 to rows - 1 do
    for y = 0 to cols - 1 do
      let live_neighbors = count_live_neighbors grid x y in
      new_grid.(x).(y) <- (grid.(x).(y) && (live_neighbors = 2 || live_neighbors = 3)) || (not grid.(x).(y) && live_neighbors = 3)
    done
  done;
  new_grid

(* Example usage *)
let () =
  let grid = [|
    [| false; true; false |];
    [| false; false; true |];
    [| true; true; true |]
  |] in
  Printf.printf "Initial grid:\n";
  print_grid grid;
  let new_grid = next_state grid in
  Printf.printf "Next state:\n";
  print_grid new_grid

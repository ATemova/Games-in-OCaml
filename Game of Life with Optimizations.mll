(* Define the grid and cell states *)
type cell = Alive | Dead
type grid = cell array array

(* Initialize the grid *)
let initialize_grid rows cols =
  Array.make_matrix rows cols Dead

(* Update the board with optimizations *)
let update_board grid =
  let new_grid = Array.copy grid in
  (* Implement optimized logic for updating grid *)
  new_grid

(* Main game loop *)
let rec game_loop grid =
  (* Handle grid updates and display *)
  ()

(* Main function to start the game *)
let () =
  let grid = initialize_grid 50 50 in
  game_loop grid

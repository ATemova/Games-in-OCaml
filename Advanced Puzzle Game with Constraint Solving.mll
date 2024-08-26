(* Advanced Puzzle Game with Constraint Solving *)

type puzzle = Sudoku | Nonogram
type game_state = { puzzles: puzzle list; solved: bool list }

let solve_puzzle (state: game_state) (puzzle: puzzle) : game_state =
  (* Use constraint solving techniques to solve puzzles *)
  state

let check_win (state: game_state) : bool =
  (* Check if all puzzles are solved *)
  List.for_all (fun solved -> solved) state.solved

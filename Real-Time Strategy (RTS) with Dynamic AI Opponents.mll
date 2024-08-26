(* RTS Game with Dynamic AI Opponents *)

type resource = Gold | Wood
type unit = Worker | Soldier | Archer
type building = Barracks | Mine
type ai_strategy = Aggressive | Defensive | Balanced
type game_state = { resources: (resource * int) list; units: (unit * (int * int)) list; buildings: (building * (int * int)) list; ai_strategy: ai_strategy }

let update_resources (state: game_state) : game_state =
  (* Update resources based on AI strategy and actions *)
  state

let create_unit (state: game_state) (unit: unit) : game_state =
  (* Create a new unit and update state *)
  state

let ai_update (state: game_state) : game_state =
  (* Update AI behavior based on strategy *)
  state

let update_state (state: game_state) : game_state =
  update_resources state;
  ai_update state;
  state

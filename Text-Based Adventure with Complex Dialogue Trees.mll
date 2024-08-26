(* Text-Based Adventure with Complex Dialogue Trees *)

type room = { description: string; exits: (string * room) list; items: string list }
type dialogue = { prompt: string; responses: (string * dialogue) list }
type game_state = { current_room: room; inventory: string list; dialogue_tree: dialogue }

let move_to_room (state: game_state) (direction: string) : game_state =
  (* Move to a new room based on direction *)
  state

let interact_with_item (state: game_state) (item: string) : game_state =
  (* Interact with an item in the inventory *)
  state

let interact_with_npc (state: game_state) (response: string) : game_state =
  (* Engage in dialogue with an NPC based on the chosen response *)
  state

let print_state (state: game_state) : unit =
  (* Print the current game state and description *)
  ()

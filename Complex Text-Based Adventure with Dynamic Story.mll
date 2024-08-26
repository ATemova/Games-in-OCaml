(* Complex Text-Based Adventure with Dynamic Story *)

type room = { description: string; exits: (string * room) list; items: string list }
type game_state = { current_room: room; inventory: string list; story_progress: int }

let move_to_room (state: game_state) (direction: string) : game_state =
  (* Move to a new room based on direction *)
  state

let interact_with_item (state: game_state) (item: string) : game_state =
  (* Interact with an item in the inventory *)
  state

let update_story (state: game_state) : game_state =
  (* Update the story based on progress and interactions *)
  state

let print_state (state: game_state) : unit =
  (* Print the current game state and description *)
  ()

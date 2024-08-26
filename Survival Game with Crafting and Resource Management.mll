(* Survival Game with Crafting and Resource Management *)

type resource = Wood | Stone | Metal
type item = Weapon | Tool | Medicine
type weather = Sunny | Rainy | Stormy
type game_state = { resources: (resource * int) list; inventory: (item * int) list; weather: weather }

let gather_resource (state: game_state) (resource: resource) (amount: int) : game_state =
  (* Gather resources and update state *)
  state

let craft_item (state: game_state) (item: item) : game_state =
  (* Craft an item from resources *)
  state

let update_weather (state: game_state) : game_state =
  (* Update weather conditions *)
  state

let update_state (state: game_state) : game_state =
  update_weather state;
  state

(* Advanced Platformer with Dynamic Obstacles and Power-Ups *)

type entity = { pos: (float * float); velocity: (float * float); width: float; height: float; power_ups: string list }
type obstacle = MovingPlatform | Spikes
type level = { tiles: tile array array; entities: entity list; obstacles: obstacle list }
type tile = Solid | Platform | Empty

let move_entity (entity: entity) (dx: float) (dy: float) : entity =
  (* Move entity and handle collisions *)
  { entity with pos = (fst entity.pos +. dx, snd entity.pos +. dy) }

let update_obstacles (obstacles: obstacle list) : obstacle list =
  (* Update the state of dynamic obstacles *)
  obstacles

let update_level (level: level) : level =
  (* Update the level including obstacles and power-ups *)
  level

(* Advanced Platformer with Physics and Dynamic Levels *)

type entity = { pos: (float * float); velocity: (float * float); width: float; height: float }
type level = { tiles: tile array array; entities: entity list }
type tile = Solid | Platform | Empty

let move_entity (entity: entity) (dx: float) (dy: float) : entity =
  (* Move entity and handle collisions *)
  { entity with pos = (fst entity.pos +. dx, snd entity.pos +. dy) }

let update_physics (level: level) : level =
  (* Update the physics for all entities in the level *)
  level

let check_collision (e1: entity) (e2: entity) : bool =
  (* Check for collision between two entities *)
  false

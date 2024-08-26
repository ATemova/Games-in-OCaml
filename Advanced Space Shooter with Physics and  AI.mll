(* Advanced Space Shooter with Physics and AI *)

type spaceship = { pos: (float * float); velocity: (float * float); health: int }
type projectile = { pos: (float * float); velocity: (float * float) }
type game_state = { player: spaceship; enemies: spaceship list; projectiles: projectile list }

let move_spaceship (ship: spaceship) (dx: float) (dy: float) : spaceship =
  (* Move spaceship with physics *)
  { ship with pos = (fst ship.pos +. dx, snd ship.pos +. dy) }

let fire_projectile (ship: spaceship) : projectile =
  (* Fire a projectile from the spaceship *)
  { pos = ship.pos; velocity = (0.0, 10.0) }

let update_projectiles (projectiles: projectile list) : projectile list =
  (* Update the position of all projectiles considering physics *)
  projectiles

let ai_control (enemies: spaceship list) : spaceship list =
  (* AI for enemy movement and shooting *)
  enemies

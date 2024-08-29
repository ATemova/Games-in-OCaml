type monster = { name: string; hp: int; attack: int }
type player = { mutable hp: int; attack: int }

let monsters = [
  { name = "Goblin"; hp = 30; attack = 5 };
  { name = "Orc"; hp = 50; attack = 10 };
  { name = "Dragon"; hp = 100; attack = 20 }
]

let encounter player =
  let monster = List.nth monsters (Random.int (List.length monsters)) in
  Printf.printf "A wild %s appears!\n" monster.name;
  let rec battle () =
    Printf.printf "Player HP: %d, %s HP: %d\n" player.hp monster.name monster.hp;
    print_endline "Do you attack (a) or flee (f)?";
    match read_line () with
    | "a" ->
      monster.hp <- monster.hp - player.attack;
      if monster.hp <= 0 then
        print_endline "You defeated the monster!"
      else
        (player.hp <- player.hp - monster.attack;
         if player.hp <= 0 then
           print_endline "You were defeated. Game over."
         else
           battle ())
    | "f" -> print_endline "You fled from the battle."
    | _ -> print_endline "Invalid action. Try again."; battle ()
  in
  battle ()

let () =
  Random.self_init ();
  let player = { hp = 100; attack = 15 } in
  print_endline "You enter a dark dungeon...";
  encounter player

let move_tower n from_peg to_peg aux_peg =
  if n = 1 then
    Printf.printf "Move disk from %s to %s\n" from_peg to_peg
  else (
    move_tower (n - 1) from_peg aux_peg to_peg;
    Printf.printf "Move disk from %s to %s\n" from_peg to_peg;
    move_tower (n - 1) aux_peg to_peg from_peg
  )

let () =
  let n = 3 in
  move_tower n "A" "C" "B"

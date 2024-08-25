(* Extended Euclidean Algorithm *)
let rec extended_gcd a b =
  if b = 0 then (a, 1, 0)
  else
    let (g, x1, y1) = extended_gcd b (a mod b) in
    let x = y1 in
    let y = x1 - (a / b) * y1 in
    (g, x, y)

(* Example usage *)
let () =
  let (g, x, y) = extended_gcd 252 198 in
  Printf.printf "GCD: %d, x: %d, y: %d\n" g x y

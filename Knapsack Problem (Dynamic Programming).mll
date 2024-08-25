(* Knapsack problem using dynamic programming *)
let knapsack values weights capacity =
  let n = Array.length values in
  let dp = Array.make_matrix (n + 1) (capacity + 1) 0 in
  for i = 1 to n do
    for w = 1 to capacity do
      if weights.(i - 1) <= w then
        dp.(i).(w) <- max dp.(i - 1).(w) (dp.(i - 1).(w - weights.(i - 1)) + values.(i - 1))
      else
        dp.(i).(w) <- dp.(i - 1).(w)
    done
  done;
  dp.(n).(capacity)

(* Example usage *)
let () =
  let values = [| 60; 100; 120 |] in
  let weights = [| 10; 20; 30 |] in
  let capacity = 50 in
  Printf.printf "Maximum value: %d\n" (knapsack values weights capacity)

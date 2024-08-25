(* Sieve of Eratosthenes to generate prime numbers *)
let sieve n =
  let is_prime = Array.make (n + 1) true in
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  for i = 2 to n do
    if is_prime.(i) then
      for j = i * i to n do
        if j mod i = 0 then is_prime.(j) <- false
      done
  done;
  Array.to_list (Array.mapi (fun i v -> if v then Some i else None) is_prime)
  |> List.filter_map Fun.id

(* Example usage *)
let () =
  let primes = sieve 100 in
  Printf.printf "Primes up to 100: %s\n" (String.concat "; " (List.map string_of_int primes))

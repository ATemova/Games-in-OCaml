(* Memoization of Fibonacci using a hash table *)
let memoize f =
  let table = Hashtbl.create 100 in
  fun x ->
    try Hashtbl.find table x
    with Not_found ->
      let result = f x in
      Hashtbl.add table x result;
      result

let rec fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

let fib_memoized = memoize fib

(* Example usage *)
let () =
  let result = fib_memoized 30 in
  Printf.printf "Fibonacci(30) = %d\n" result

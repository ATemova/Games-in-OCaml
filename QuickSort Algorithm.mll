(* QuickSort implementation *)
let rec quicksort = function
  | [] -> []
  | pivot :: rest ->
    let less, greater = List.partition (fun x -> x < pivot) rest in
    quicksort less @ (pivot :: quicksort greater)

(* Example usage *)
let () =
  let list = [3; 6; 8; 10; 1; 2; 1] in
  let sorted_list = quicksort list in
  Printf.printf "Sorted list: %s\n" (String.concat "; " (List.map string_of_int sorted_list))
